

module eos_sensor


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
 !                         eos_sensor
 !          
 !          Purpose:
 !                        Various characteristics of Electro-Optical Sensors   
 !                        Based mainly on Based mainly on Miroshenko M.M book (rus):          
 !                        "Mathematical Theory of Electro-Optical Sensors".
 !          History:
 !                        Date: 09-08-2022
 !                        Time: 09:44 GMT+2
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
   !use mod_vectypes, only : YMM8r4_t, YMM4r8_t, ZMM16r4_t, ZMM8r8_t
   public
   implicit none

     ! Major version
     integer(kind=i4),  parameter :: EOS_SENSOR_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: EOS_SENSOR_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: EOS_SENSOR_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: EOS_SENSOR_FULLVER =   &
            1000*EOS_SENSOR_MAJOR+100*EOS_SENSOR_MINOR+10*EOS_SENSOR_MICRO
     ! Module creation date
     character(*),        parameter :: EOS_SENSOR_CREATE_DATE = "09-08-2022 09:44 +00200 (TUE 09 AUG 2022 GMT+2)"
     ! Module build date
     character(*),        parameter :: EOS_SENSOR_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: EOS_SENSOR_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: EOS_SENSOR_SYNOPSIS    = "EO Sensors characteristics and models."


     ! Scanning mirror derived type
     
    ! type, public :: scanning_mirror
    !
    !      sequence
    !       real(kind=sp) :: gamma ! angle of mirror position relative to obiective optical axis (p. 53, 1.2)
    !       real(kind=sp) :: gamma0 ! angle of fixing
    !       real(kind=sp) :: phi   ! sensor fov
    !       real(kind=sp) :: F     ! Focal length
    !       real(kind=sp) :: H     ! distance from the focal length to target image
    !       real(kind=sp) :: Dmax  ! size of mirror vertical plane
    !       real(kind=sp) :: Dmin  ! size of mirror horizontal plane
    !       
    ! end type scanning_mirror


     contains

     
     subroutine param_gamma_r4(phi,gamma)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: param_gamma_r4
        !dir$ attributes forceinline :: param_gamma_r4
        real(kind=sp), intent(in)    :: phi
        real(kind=sp), intent(out)   :: gama
        gamma = 0.5_sp*phi*0.5_sp
     end subroutine param_gamma_r4


     subroutine param_gamma_unroll_16x_r4(phi,gamma,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  param_gamma_unroll_16x_r4
           !dir$ attributes forceinline ::  param_gamma_unroll_16x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: param_gamma_unroll_16x_r4
           real(kind=sp), dimension(1:n), intent(in)  :: phi
           real(kind=sp), dimension(1:n), intent(out) :: gamma
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=sp), automatic :: t8,t9,t10,t11,t12,t13,t14,t15
        
           integer(kind=i4) :: i,m,m1
           m = mod(n,16)
           if(m /= 0) then
              do i=1,m
                 call param_gamma_r4(phi(i),gamma(i))
              end do
              if(n<16) return
            end if
            m1 = m+1
            !dir$ assume_aligned phi:64
            !dir$ assume_aligned gamma:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,16
               t0 = phi(i)
               call param_gamma_r4(t0,gamma(i))
               t1 = phi(i+1)
               call param_gamma_r4(t1,gamma(i+1))
               t2 = phi(i+2)
               call param_gamma_r4(t2,gamma(i+2))
               t3 = phi(i+3)
               call param_gamma_r4(t3,gamma(i+3))
               t4 = phi(i+4)
               call param_gamma_r4(t4,gamma(i+4))
               t5 = phi(i+5)
               call param_gamma_r4(t5,gamma(i+5))
               t6 = phi(i+6)
               call param_gamma_r4(t6,gamma(i+6))
               t7 = phi(i+7)
               call param_gamma_r4(t7,gamma(i+7))
               t8 = phi(i+8)
               call param_gamma_r4(t8,gamma(i+8))
               t9 = phi(i+9)
               call param_gamma_r4(t9,gamma(i+9))
               t10= phi(i+10)
               call param_gamma_r4(t10,gamma(i+10))
               t11= phi(i+11)
               call param_gamma_r4(t11,gamma(i+11))
               t12= phi(i+12)
               call param_gamma_r4(t12,gamma(i+12))
               t13= phi(i+13)
               call param_gamma_r4(t13,gamma(i+13))
               t14= phi(i+14)
               call param_gamma_r4(t14,gamma(i+14))
               t15= phi(i+15)
               call param_gamma_r4(t15,gamma(i+15))
             end do
     end subroutine param_gamma_unroll_16x_r4


     subroutine param_gamma_unroll_8x_r4(phi,gamma,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  param_gamma_unroll_8x_r4
           !dir$ attributes forceinline ::  param_gamma_unroll_8x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: param_gamma_unroll_8x_r4
           real(kind=sp), dimension(1:n), intent(in)  :: phi
           real(kind=sp), dimension(1:n), intent(out) :: gamma
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7
                 
           integer(kind=i4) :: i,m,m1
           m = mod(n,8)
           if(m /= 0) then
              do i=1,m
                 call param_gamma_r4(phi(i),gamma(i))
              end do
              if(n<8) return
            end if
            m1 = m+1
            !dir$ assume_aligned phi:64
            !dir$ assume_aligned gamma:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,8
               t0 = phi(i)
               call param_gamma_r4(t0,gamma(i))
               t1 = phi(i+1)
               call param_gamma_r4(t1,gamma(i+1))
               t2 = phi(i+2)
               call param_gamma_r4(t2,gamma(i+2))
               t3 = phi(i+3)
               call param_gamma_r4(t3,gamma(i+3))
               t4 = phi(i+4)
               call param_gamma_r4(t4,gamma(i+4))
               t5 = phi(i+5)
               call param_gamma_r4(t5,gamma(i+5))
               t6 = phi(i+6)
               call param_gamma_r4(t6,gamma(i+6))
               t7 = phi(i+7)
               call param_gamma_r4(t7,gamma(i+7))
             end do
     end subroutine param_gamma_unroll_8x_r4


     subroutine param_gamma_unroll_4x_r4(phi,gamma,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  param_gamma_unroll_4x_r4
           !dir$ attributes forceinline ::  param_gamma_unroll_4x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: param_gamma_unroll_4x_r4
           real(kind=sp), dimension(1:n), intent(in)  :: phi
           real(kind=sp), dimension(1:n), intent(out) :: gamma
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp), automatic :: t0,t1,t2,t3
                 
           integer(kind=i4) :: i,m,m1
           m = mod(n,4)
           if(m /= 0) then
              do i=1,m
                 call param_gamma_r4(phi(i),gamma(i))
              end do
              if(n<4) return
            end if
            m1 = m+1
            !dir$ assume_aligned phi:64
            !dir$ assume_aligned gamma:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,4
               t0 = phi(i)
               call param_gamma_r4(t0,gamma(i))
               t1 = phi(i+1)
               call param_gamma_r4(t1,gamma(i+1))
               t2 = phi(i+2)
               call param_gamma_r4(t2,gamma(i+2))
               t3 = phi(i+3)
               call param_gamma_r4(t3,gamma(i+3))
           end do
     end subroutine param_gamma_unroll_4x_r4


     
    subroutine param_gamma_unroll_2x_r4(phi,gamma,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  param_gamma_unroll_2x_r4
           !dir$ attributes forceinline ::  param_gamma_unroll_2x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: param_gamma_unroll_2x_r4
           real(kind=sp), dimension(1:n), intent(in)  :: phi
           real(kind=sp), dimension(1:n), intent(out) :: gamma
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp), automatic :: t0,t1
                 
           integer(kind=i4) :: i,m,m1
           m = mod(n,2)
           if(m /= 0) then
              do i=1,m
                 call param_gamma_r4(phi(i),gamma(i))
              end do
              if(n<2) return
            end if
            m1 = m+1
            !dir$ assume_aligned phi:64
            !dir$ assume_aligned gamma:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,2
               t0 = phi(i)
               call param_gamma_r4(t0,gamma(i))
               t1 = phi(i+1)
               call param_gamma_r4(t1,gamma(i+1))
            
           end do
     end subroutine param_gamma_unroll_2x_r4


     subroutine param_gamma_rolled_r4(phi,gamma,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  param_gamma_rolled_r4
           !dir$ attributes forceinline ::  param_gamma_rolled_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: param_gamma_rolled_r4
           real(kind=sp), dimension(1:n), intent(in)  :: phi
           real(kind=sp), dimension(1:n), intent(out) :: gamma
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp), automatic :: t0
                 
           integer(kind=i4) :: i
       
            !dir$ assume_aligned phi:64
            !dir$ assume_aligned gamma:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=1,n
               t0 = phi(i)
               call param_gamma_r4(t0,gamma(i))
            end do
     end subroutine param_gamma_rolled_r4


     subroutine param_gamma_dispatch_r4(phi,gamma,n,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: param_gamma_dispatch_r4
           real(kind=sp), dimension(1:n), intent(in)  :: phi
           real(kind=sp), dimension(1:n), intent(out) :: gamma
           integer(kind=i4),              intent(in)  :: n
           integer(kind=i4),              intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                call param_gamma_unroll_16x_r4(phi,gamma,n)
              case (8)
                call param_gamma_unroll_8x_r4(phi,gamma,n)
              case (4)
                call param_gamma_unroll_4x_r4(phi,gamma,n)
              case (2)
                call param_gamma_unroll_2x_r4(phi,gamma,n)
              case (0)
                call param_gamma_rolled_r4(phi,gamma,n)
              case default
                return
            end select
     end subroutine param_gamma_dispatch_r4
     


     

   
     subroutine param_gamma_r8(phi,gamma)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: param_gamma_r8
        !dir$ attributes forceinline :: param_gamma_r8
        real(kind=dp), intent(in)    :: phi
        real(kind=dp), intent(out)   :: gama
        gamma = 0.5_dp*phi*0.5_dp
     end subroutine param_gamma_r8


     subroutine param_gamma_unroll_16x_r8(phi,gamma,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  param_gamma_unroll_16x_r8
           !dir$ attributes forceinline ::  param_gamma_unroll_16x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: param_gamma_unroll_16x_r8
           real(kind=dp), dimension(1:n), intent(in)  :: phi
           real(kind=dp), dimension(1:n), intent(out) :: gamma
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=dp), automatic :: t8,t9,t10,t11,t12,t13,t14,t15
        
           integer(kind=i4) :: i,m,m1
           m = mod(n,16)
           if(m /= 0) then
              do i=1,m
                 call param_gamma_r8(phi(i),gamma(i))
              end do
              if(n<16) return
            end if
            m1 = m+1
            !dir$ assume_aligned phi:64
            !dir$ assume_aligned gamma:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,16
               t0 = phi(i)
               call param_gamma_r8(t0,gamma(i))
               t1 = phi(i+1)
               call param_gamma_r8(t1,gamma(i+1))
               t2 = phi(i+2)
               call param_gamma_r8(t2,gamma(i+2))
               t3 = phi(i+3)
               call param_gamma_r8(t3,gamma(i+3))
               t4 = phi(i+4)
               call param_gamma_r8(t4,gamma(i+4))
               t5 = phi(i+5)
               call param_gamma_r8(t5,gamma(i+5))
               t6 = phi(i+6)
               call param_gamma_r8(t6,gamma(i+6))
               t7 = phi(i+7)
               call param_gamma_r8(t7,gamma(i+7))
               t8 = phi(i+8)
               call param_gamma_r8(t8,gamma(i+8))
               t9 = phi(i+9)
               call param_gamma_r8(t9,gamma(i+9))
               t10= phi(i+10)
               call param_gamma_r8(t10,gamma(i+10))
               t11= phi(i+11)
               call param_gamma_r8(t11,gamma(i+11))
               t12= phi(i+12)
               call param_gamma_r8(t12,gamma(i+12))
               t13= phi(i+13)
               call param_gamma_r8(t13,gamma(i+13))
               t14= phi(i+14)
               call param_gamma_r8(t14,gamma(i+14))
               t15= phi(i+15)
               call param_gamma_r8(t15,gamma(i+15))
             end do
     end subroutine param_gamma_unroll_16x_r8


     subroutine param_gamma_unroll_8x_r8(phi,gamma,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  param_gamma_unroll_8x_r8
           !dir$ attributes forceinline ::  param_gamma_unroll_8x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: param_gamma_unroll_8x_r8
           real(kind=dp), dimension(1:n), intent(in)  :: phi
           real(kind=dp), dimension(1:n), intent(out) :: gamma
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7
                 
           integer(kind=i4) :: i,m,m1
           m = mod(n,8)
           if(m /= 0) then
              do i=1,m
                 call param_gamma_r8(phi(i),gamma(i))
              end do
              if(n<8) return
            end if
            m1 = m+1
            !dir$ assume_aligned phi:64
            !dir$ assume_aligned gamma:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,8
               t0 = phi(i)
               call param_gamma_r8(t0,gamma(i))
               t1 = phi(i+1)
               call param_gamma_r8(t1,gamma(i+1))
               t2 = phi(i+2)
               call param_gamma_r8(t2,gamma(i+2))
               t3 = phi(i+3)
               call param_gamma_r8(t3,gamma(i+3))
               t4 = phi(i+4)
               call param_gamma_r8(t4,gamma(i+4))
               t5 = phi(i+5)
               call param_gamma_r8(t5,gamma(i+5))
               t6 = phi(i+6)
               call param_gamma_r8(t6,gamma(i+6))
               t7 = phi(i+7)
               call param_gamma_r8(t7,gamma(i+7))
             end do
     end subroutine param_gamma_unroll_8x_r8


     subroutine param_gamma_unroll_4x_r8(phi,gamma,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  param_gamma_unroll_4x_r8
           !dir$ attributes forceinline ::  param_gamma_unroll_4x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: param_gamma_unroll_4x_r8
           real(kind=dp), dimension(1:n), intent(in)  :: phi
           real(kind=dp), dimension(1:n), intent(out) :: gamma
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp), automatic :: t0,t1,t2,t3
                 
           integer(kind=i4) :: i,m,m1
           m = mod(n,4)
           if(m /= 0) then
              do i=1,m
                 call param_gamma_r8(phi(i),gamma(i))
              end do
              if(n<4) return
            end if
            m1 = m+1
            !dir$ assume_aligned phi:64
            !dir$ assume_aligned gamma:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,4
               t0 = phi(i)
               call param_gamma_r8(t0,gamma(i))
               t1 = phi(i+1)
               call param_gamma_r8(t1,gamma(i+1))
               t2 = phi(i+2)
               call param_gamma_r8(t2,gamma(i+2))
               t3 = phi(i+3)
               call param_gamma_r8(t3,gamma(i+3))
           end do
     end subroutine param_gamma_unroll_4x_r8


     
    subroutine param_gamma_unroll_2x_r8(phi,gamma,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  param_gamma_unroll_2x_r8
           !dir$ attributes forceinline ::  param_gamma_unroll_2x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: param_gamma_unroll_2x_r8
           real(kind=dp), dimension(1:n), intent(in)  :: phi
           real(kind=dp), dimension(1:n), intent(out) :: gamma
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp), automatic :: t0,t1
                 
           integer(kind=i4) :: i,m,m1
           m = mod(n,2)
           if(m /= 0) then
              do i=1,m
                 call param_gamma_r8(phi(i),gamma(i))
              end do
              if(n<2) return
            end if
            m1 = m+1
            !dir$ assume_aligned phi:64
            !dir$ assume_aligned gamma:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,2
               t0 = phi(i)
               call param_gamma_r8(t0,gamma(i))
               t1 = phi(i+1)
               call param_gamma_r8(t1,gamma(i+1))
            
           end do
     end subroutine param_gamma_unroll_2x_r8


     subroutine param_gamma_rolled_r8(phi,gamma,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  param_gamma_rolled_r8
           !dir$ attributes forceinline ::  param_gamma_rolled_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: param_gamma_rolled_r8
           real(kind=dp), dimension(1:n), intent(in)  :: phi
           real(kind=dp), dimension(1:n), intent(out) :: gamma
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp), automatic :: t0
                 
           integer(kind=i4) :: i
       
            !dir$ assume_aligned phi:64
            !dir$ assume_aligned gamma:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=1,n
               t0 = phi(i)
               call param_gamma_r8(t0,gamma(i))
            end do
     end subroutine param_gamma_rolled_r8


     subroutine param_gamma_dispatch_r8(phi,gamma,n,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: param_gamma_dispatch_r8
           real(kind=dp), dimension(1:n), intent(in)  :: phi
           real(kind=dp), dimension(1:n), intent(out) :: gamma
           integer(kind=i4),              intent(in)  :: n
           integer(kind=i4),              intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                call param_gamma_unroll_16x_r8(phi,gamma,n)
              case (8)
                call param_gamma_unroll_8x_r8(phi,gamma,n)
              case (4)
                call param_gamma_unroll_4x_r8(phi,gamma,n)
              case (2)
                call param_gamma_unroll_2x_r8(phi,gamma,n)
              case (0)
                call param_gamma_rolled_r8(phi,gamma,n)
              case default
                return
            end select
     end subroutine param_gamma_dispatch_r8
     


     ! Formula 1, p.54
     !Тогда длина перпендикуляра SN, опущенного из 
     !светящейся точки на плоскость зеркала
     pure elemental function compute_SN_r4(R,phi,gamma) result(SN)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_r4
        !dir$ attributes forceinline :: compute_SN_r4
        real(kind=sp),  intent(in) :: R
        real(kind=sp),  intent(in), optional :: phi
        real(kind=sp),  intent(in), optional :: gamma
        real(kind=sp) :: SN
        if(present(phi)) then
            SN = R*sin(phi)
        else if(present(gamma)) then
            SN = R*cos(gamma)
        end if
     end function compute_SN_r4


     subroutine compute_SN_unroll_16x_r4(R,phi,gamma,n,SN)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_SN_unroll_16x_r4
           !dir$ attributes forceinline ::  compute_SN_unroll_16x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_16x_r4
           real(kind=sp),                 intent(in) :: R
           real(kind=sp), dimension(1:n), intent(in) :: phi
           real(kind=sp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=sp), dimension(1:n), intent(out):: SN
           real(kind=sp), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
           real(kind=sp), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
           real(kind=sp), automatic :: g0,g1,g2,g3,g4,g5,g6,g7
           real(kind=sp), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
           real(kind=sp), automatic :: s0,s1,s2,s3,s4,s5,s6,s7
           real(kind=sp), automatic :: s8,s9,s10,s11,s12,s13,s14,s15
           integer(kind=i4) :: i,m,m1
           m = mod(n,16)
           if(m /= 0) then
              do i=1,m
                 s0 = compute_SN_r4(R,phi(i),gamma(i))
                 SN(i) = s0
              end do
              if(n<16) return
            end if
            m1 = m+1
            !dir$ assume_aligned phi:64
            !dir$ assume_aligned gamma:64
            !dir$ assume_aligned SN:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,16
                p0      = phi(i)
                g0      = gamma(i)
                s0      = compute_SN_r4(R,p0,g0)
                SN(i)   = s0
                p1      = phi(i+1)
                g1      = gamma(i+1)
                s1      = compute_SN_r4(R,p1,g1)
                SN(i+1) = s1
                p2      = phi(i+2)
                g2      = gamma(i+2)
                s2      = compute_SN_r4(R,p2,g2)
                SN(i+2) = s2
                p3      = phi(i+3)
                g3      = gamma(i+3)
                s3      = compute_SN_r4(R,p3,g3)
                SN(i+3) = s3
                p4      = phi(i+4)
                g4      = gamma(i+4)
                s4      = compute_SN_r4(R,p4,g4)
                SN(i+4) = s4
                p5      = phi(i+5)
                g5      = gamma(i+5)
                s5      = compute_SN_r4(R,p5,g5)
                SN(i+5) = s5
                p6      = phi(i+6)
                g6      = gamma(i+6)
                s6      = compute_SN_r4(R,p6,g6)
                SN(i+6) = s6
                p7      = phi(i+7)
                g7      = gamma(i+7)
                s7      = compute_SN_r4(R,p7,g7)
                SN(i+7) = s7
                p8      = phi(i+8)
                g8      = gamma(i+8)
                s8      = compute_SN_r4(R,p8,g8)
                SN(i+8) = s8
                p9      = phi(i+9)
                g9      = gamma(i+9)
                s9      = compute_SN_r4(R,p9,g9)
                SN(i+9) = s9
                p10     = phi(i+10)
                g10     = gamma(i+10)
                s10     = compute_SN_r4(R,p10,g10)
                SN(i+10)= s10
                p11     = phi(i+11)
                g11     = gamma(i+11)
                s11     = compute_SN_r4(R,p11,g11)
                SN(i+11)= s11
                p12     = phi(i+12)
                g12     = gamma(i+12)
                s12     = compute_SN_r4(R,p12,g12)
                SN(i+12)= s12
                p13     = phi(i+13)
                g13     = gamma(i+13)
                s13     = compute_SN_r4(R,p13,g13)
                SN(i+13)= s13
                p14     = phi(i+14)
                g14     = gamma(i+14)
                s14     = compute_SN_r4(R,p14,g14)
                SN(i+14)= s14
                p15     = phi(i+15)
                g15     = gamma(i+15)
                s15     = compute_SN_r4(R,p15,g15)
                SN(i+15)= s15
            end do
     end subroutine compute_SN_unroll_16x_r4


     subroutine compute_SN_unroll_8x_r4(R,phi,gamma,n,SN)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_SN_unroll_8x_r4
           !dir$ attributes forceinline ::  compute_SN_unroll_8x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_8x_r4
           real(kind=sp),                 intent(in) :: R
           real(kind=sp), dimension(1:n), intent(in) :: phi
           real(kind=sp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=sp), dimension(1:n), intent(out):: SN
           real(kind=sp), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
           real(kind=sp), automatic :: g0,g1,g2,g3,g4,g5,g6,g7
           real(kind=sp), automatic :: s0,s1,s2,s3,s4,s5,s6,s7
           integer(kind=i4) :: i,m,m1
           m = mod(n,8)
           if(m /= 0) then
              do i=1,m
                 s0 = compute_SN_r4(R,phi(i),gamma(i))
                 SN(i) = s0
              end do
              if(n<8) return
            end if
            m1 = m+1
            !dir$ assume_aligned phi:64
            !dir$ assume_aligned gamma:64
            !dir$ assume_aligned SN:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,8
                p0      = phi(i)
                g0      = gamma(i)
                s0      = compute_SN_r4(R,p0,g0)
                SN(i)   = s0
                p1      = phi(i+1)
                g1      = gamma(i+1)
                s1      = compute_SN_r4(R,p1,g1)
                SN(i+1) = s1
                p2      = phi(i+2)
                g2      = gamma(i+2)
                s2      = compute_SN_r4(R,p2,g2)
                SN(i+2) = s2
                p3      = phi(i+3)
                g3      = gamma(i+3)
                s3      = compute_SN_r4(R,p3,g3)
                SN(i+3) = s3
                p4      = phi(i+4)
                g4      = gamma(i+4)
                s4      = compute_SN_r4(R,p4,g4)
                SN(i+4) = s4
                p5      = phi(i+5)
                g5      = gamma(i+5)
                s5      = compute_SN_r4(R,p5,g5)
                SN(i+5) = s5
                p6      = phi(i+6)
                g6      = gamma(i+6)
                s6      = compute_SN_r4(R,p6,g6)
                SN(i+6) = s6
                p7      = phi(i+7)
                g7      = gamma(i+7)
                s7      = compute_SN_r4(R,p7,g7)
                SN(i+7) = s7
              end do
     end subroutine compute_SN_unroll_8x_r4


     subroutine compute_SN_unroll_4x_r4(R,phi,gamma,n,SN)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_SN_unroll_4x_r4
           !dir$ attributes forceinline ::  compute_SN_unroll_4x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_4x_r4
           real(kind=sp),                 intent(in) :: R
           real(kind=sp), dimension(1:n), intent(in) :: phi
           real(kind=sp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=sp), dimension(1:n), intent(out):: SN
           real(kind=sp), automatic :: p0,p1,p2,p3
           real(kind=sp), automatic :: g0,g1,g2,g3
           real(kind=sp), automatic :: s0,s1,s2,s3
           integer(kind=i4) :: i,m,m1
           m = mod(n,4)
           if(m /= 0) then
              do i=1,m
                 s0 = compute_SN_r4(R,phi(i),gamma(i))
                 SN(i) = s0
              end do
              if(n<4) return
            end if
            m1 = m+1
            !dir$ assume_aligned phi:64
            !dir$ assume_aligned gamma:64
            !dir$ assume_aligned SN:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,4
                p0      = phi(i)
                g0      = gamma(i)
                s0      = compute_SN_r4(R,p0,g0)
                SN(i)   = s0
                p1      = phi(i+1)
                g1      = gamma(i+1)
                s1      = compute_SN_r4(R,p1,g1)
                SN(i+1) = s1
                p2      = phi(i+2)
                g2      = gamma(i+2)
                s2      = compute_SN_r4(R,p2,g2)
                SN(i+2) = s2
                p3      = phi(i+3)
                g3      = gamma(i+3)
                s3      = compute_SN_r4(R,p3,g3)
                SN(i+3) = s3
              end do
     end subroutine compute_SN_unroll_4x_r4


     subroutine compute_SN_unroll_2x_r4(R,phi,gamma,n,SN)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_SN_unroll_2x_r4
           !dir$ attributes forceinline ::  compute_SN_unroll_2x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_2x_r4
           real(kind=sp),                 intent(in) :: R
           real(kind=sp), dimension(1:n), intent(in) :: phi
           real(kind=sp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=sp), dimension(1:n), intent(out):: SN
           real(kind=sp), automatic :: p0,p1
           real(kind=sp), automatic :: g0,g1
           real(kind=sp), automatic :: s0,s1
           integer(kind=i4) :: i,m,m1
           m = mod(n,2)
           if(m /= 0) then
              do i=1,m
                 s0 = compute_SN_r4(R,phi(i),gamma(i))
                 SN(i) = s0
              end do
              if(n<2) return
            end if
            m1 = m+1
            !dir$ assume_aligned phi:64
            !dir$ assume_aligned gamma:64
            !dir$ assume_aligned SN:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,2
                p0      = phi(i)
                g0      = gamma(i)
                s0      = compute_SN_r4(R,p0,g0)
                SN(i)   = s0
                p1      = phi(i+1)
                g1      = gamma(i+1)
                s1      = compute_SN_r4(R,p1,g1)
                SN(i+1) = s1
               
              end do
     end subroutine compute_SN_unroll_2x_r4


     subroutine compute_SN_rolled_r4(R,phi,gamma,n,SN)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_SN_rolled_r4
           !dir$ attributes forceinline ::  compute_SN_rolled_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_rolled_r4
           real(kind=sp),                 intent(in) :: R
           real(kind=sp), dimension(1:n), intent(in) :: phi
           real(kind=sp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=sp), dimension(1:n), intent(out):: SN
           real(kind=sp), automatic :: p0
           real(kind=sp), automatic :: g0
           real(kind=sp), automatic :: s0
           integer(kind=i4) :: i
          
            !dir$ assume_aligned phi:64
            !dir$ assume_aligned gamma:64
            !dir$ assume_aligned SN:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=1,n
                p0      = phi(i)
                g0      = gamma(i)
                s0      = compute_SN_r4(R,p0,g0)
                SN(i)   = s0
                p1      = phi(i+1)
                g1      = gamma(i+1)
                s1      = compute_SN_r4(R,p1,g1)
                SN(i+1) = s1
               
              end do
     end subroutine compute_SN_rolled_r4


     subroutine compute_SN_dispatch_r4(R,phi,gamma,n,SN,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: compute_SN_dispatch_r4
           real(kind=sp),                 intent(in) :: R
           real(kind=sp), dimension(1:n), intent(in) :: phi
           real(kind=sp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=sp), dimension(1:n), intent(out):: SN
           integer(kind=i4),              intent(in) :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                 call compute_SN_unroll_16x_r4(R,phi,gamma,n,SN)
              case (8)
                 call compute_SN_unroll_8x_r4(R,phi,gamma,n,SN)
              case (4)
                 call compute_SN_unroll_4x_r4(R,phi,gamma,n,SN)
              case (2)
                 call compute_SN_unroll_2x_r4(R,phi,gamma,n,SN)
              case (0)
                 call compute_SN_rolled_r4(R,phi,gamma,n,SN)
              case default
                 return
           end select
     end subroutine compute_SN_dispatch_r4

     
 




     




     


     pure elemental function compute_SN_r8(R,phi,gamma) result(SN)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_r8
        !dir$ attributes forceinline :: compute_SN_r8
        real(kind=dp),  intent(in) :: R
        real(kind=dp),  intent(in), optional :: phi
        real(kind=dp),  intent(in), optional :: gamma
        real(kind=dp) :: SN
        if(present(phi)) then
            SN = R*sin(phi)
        else if(present(gamma)) then
            SN = R*cos(gamma)
        end if
     end function compute_SN_r8


     subroutine compute_SN_unroll_16x_r8(R,phi,gamma,n,SN)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_SN_unroll_16x_r8
           !dir$ attributes forceinline ::  compute_SN_unroll_16x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_16x_r8
           real(kind=dp),                 intent(in) :: R
           real(kind=dp), dimension(1:n), intent(in) :: phi
           real(kind=dp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=dp), dimension(1:n), intent(out):: SN
           real(kind=dp), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
           real(kind=dp), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
           real(kind=dp), automatic :: g0,g1,g2,g3,g4,g5,g6,g7
           real(kind=dp), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
           real(kind=dp), automatic :: s0,s1,s2,s3,s4,s5,s6,s7
           real(kind=dp), automatic :: s8,s9,s10,s11,s12,s13,s14,s15
           integer(kind=i4) :: i,m,m1
           m = mod(n,16)
           if(m /= 0) then
              do i=1,m
                 s0 = compute_SN_r8(R,phi(i),gamma(i))
                 SN(i) = s0
              end do
              if(n<16) return
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
            do i=m1,n,16
                p0      = phi(i)
                g0      = gamma(i)
                s0      = compute_SN_r8(R,p0,g0)
                SN(i)   = s0
                p1      = phi(i+1)
                g1      = gamma(i+1)
                s1      = compute_SN_r8(R,p1,g1)
                SN(i+1) = s1
                p2      = phi(i+2)
                g2      = gamma(i+2)
                s2      = compute_SN_r8(R,p2,g2)
                SN(i+2) = s2
                p3      = phi(i+3)
                g3      = gamma(i+3)
                s3      = compute_SN_r8(R,p3,g3)
                SN(i+3) = s3
                p4      = phi(i+4)
                g4      = gamma(i+4)
                s4      = compute_SN_r8(R,p4,g4)
                SN(i+4) = s4
                p5      = phi(i+5)
                g5      = gamma(i+5)
                s5      = compute_SN_r8(R,p5,g5)
                SN(i+5) = s5
                p6      = phi(i+6)
                g6      = gamma(i+6)
                s6      = compute_SN_r8(R,p6,g6)
                SN(i+6) = s6
                p7      = phi(i+7)
                g7      = gamma(i+7)
                s7      = compute_SN_r8(R,p7,g7)
                SN(i+7) = s7
                p8      = phi(i+8)
                g8      = gamma(i+8)
                s8      = compute_SN_r8(R,p8,g8)
                SN(i+8) = s8
                p9      = phi(i+9)
                g9      = gamma(i+9)
                s9      = compute_SN_r8(R,p9,g9)
                SN(i+9) = s9
                p10     = phi(i+10)
                g10     = gamma(i+10)
                s10     = compute_SN_r8(R,p10,g10)
                SN(i+10)= s10
                p11     = phi(i+11)
                g11     = gamma(i+11)
                s11     = compute_SN_r8(R,p11,g11)
                SN(i+11)= s11
                p12     = phi(i+12)
                g12     = gamma(i+12)
                s12     = compute_SN_r8(R,p12,g12)
                SN(i+12)= s12
                p13     = phi(i+13)
                g13     = gamma(i+13)
                s13     = compute_SN_r8(R,p13,g13)
                SN(i+13)= s13
                p14     = phi(i+14)
                g14     = gamma(i+14)
                s14     = compute_SN_r8(R,p14,g14)
                SN(i+14)= s14
                p15     = phi(i+15)
                g15     = gamma(i+15)
                s15     = compute_SN_r8(R,p15,g15)
                SN(i+15)= s15
            end do
     end subroutine compute_SN_unroll_16x_r8


     subroutine compute_SN_unroll_8x_r8(R,phi,gamma,n,SN)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_SN_unroll_8x_r8
           !dir$ attributes forceinline ::  compute_SN_unroll_8x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_8x_r8
           real(kind=dp),                 intent(in) :: R
           real(kind=dp), dimension(1:n), intent(in) :: phi
           real(kind=dp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=dp), dimension(1:n), intent(out):: SN
           real(kind=dp), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
           real(kind=dp), automatic :: g0,g1,g2,g3,g4,g5,g6,g7
           real(kind=dp), automatic :: s0,s1,s2,s3,s4,s5,s6,s7
           integer(kind=i4) :: i,m,m1
           m = mod(n,8)
           if(m /= 0) then
              do i=1,m
                 s0 = compute_SN_r8(R,phi(i),gamma(i))
                 SN(i) = s0
              end do
              if(n<8) return
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
            do i=m1,n,8
                p0      = phi(i)
                g0      = gamma(i)
                s0      = compute_SN_r8(R,p0,g0)
                SN(i)   = s0
                p1      = phi(i+1)
                g1      = gamma(i+1)
                s1      = compute_SN_r8(R,p1,g1)
                SN(i+1) = s1
                p2      = phi(i+2)
                g2      = gamma(i+2)
                s2      = compute_SN_r8(R,p2,g2)
                SN(i+2) = s2
                p3      = phi(i+3)
                g3      = gamma(i+3)
                s3      = compute_SN_r8(R,p3,g3)
                SN(i+3) = s3
                p4      = phi(i+4)
                g4      = gamma(i+4)
                s4      = compute_SN_r8(R,p4,g4)
                SN(i+4) = s4
                p5      = phi(i+5)
                g5      = gamma(i+5)
                s5      = compute_SN_r8(R,p5,g5)
                SN(i+5) = s5
                p6      = phi(i+6)
                g6      = gamma(i+6)
                s6      = compute_SN_r8(R,p6,g6)
                SN(i+6) = s6
                p7      = phi(i+7)
                g7      = gamma(i+7)
                s7      = compute_SN_r8(R,p7,g7)
                SN(i+7) = s7
              end do
     end subroutine compute_SN_unroll_8x_r8


     subroutine compute_SN_unroll_4x_r8(R,phi,gamma,n,SN)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_SN_unroll_4x_r8
           !dir$ attributes forceinline ::  compute_SN_unroll_4x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_4x_r8
           real(kind=dp),                 intent(in) :: R
           real(kind=dp), dimension(1:n), intent(in) :: phi
           real(kind=dp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=dp), dimension(1:n), intent(out):: SN
           real(kind=dp), automatic :: p0,p1,p2,p3
           real(kind=dp), automatic :: g0,g1,g2,g3
           real(kind=dp), automatic :: s0,s1,s2,s3
           integer(kind=i4) :: i,m,m1
           m = mod(n,4)
           if(m /= 0) then
              do i=1,m
                 s0 = compute_SN_r8(R,phi(i),gamma(i))
                 SN(i) = s0
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
            do i=m1,n,4
                p0      = phi(i)
                g0      = gamma(i)
                s0      = compute_SN_r8(R,p0,g0)
                SN(i)   = s0
                p1      = phi(i+1)
                g1      = gamma(i+1)
                s1      = compute_SN_r8(R,p1,g1)
                SN(i+1) = s1
                p2      = phi(i+2)
                g2      = gamma(i+2)
                s2      = compute_SN_r8(R,p2,g2)
                SN(i+2) = s2
                p3      = phi(i+3)
                g3      = gamma(i+3)
                s3      = compute_SN_r8(R,p3,g3)
                SN(i+3) = s3
              end do
     end subroutine compute_SN_unroll_4x_r8


     subroutine compute_SN_unroll_2x_r8(R,phi,gamma,n,SN)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_SN_unroll_2x_r8
           !dir$ attributes forceinline ::  compute_SN_unroll_2x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_2x_r8
           real(kind=dp),                 intent(in) :: R
           real(kind=dp), dimension(1:n), intent(in) :: phi
           real(kind=dp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=dp), dimension(1:n), intent(out):: SN
           real(kind=dp), automatic :: p0,p1
           real(kind=dp), automatic :: g0,g1
           real(kind=dp), automatic :: s0,s1
           integer(kind=i4) :: i,m,m1
           m = mod(n,2)
           if(m /= 0) then
              do i=1,m
                 s0 = compute_SN_r8(R,phi(i),gamma(i))
                 SN(i) = s0
              end do
              if(n<2) return
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
            do i=m1,n,2
                p0      = phi(i)
                g0      = gamma(i)
                s0      = compute_SN_r8(R,p0,g0)
                SN(i)   = s0
                p1      = phi(i+1)
                g1      = gamma(i+1)
                s1      = compute_SN_r8(R,p1,g1)
                SN(i+1) = s1
               
              end do
     end subroutine compute_SN_unroll_2x_r8


     subroutine compute_SN_rolled_r8(R,phi,gamma,n,SN)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_SN_rolled_r8
           !dir$ attributes forceinline ::  compute_SN_rolled_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_rolled_r8
           real(kind=dp),                 intent(in) :: R
           real(kind=dp), dimension(1:n), intent(in) :: phi
           real(kind=dp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=dp), dimension(1:n), intent(out):: SN
           real(kind=dp), automatic :: p0
           real(kind=dp), automatic :: g0
           real(kind=dp), automatic :: s0
           integer(kind=i4) :: i
          
            !dir$ assume_aligned phi:64
            !dir$ assume_aligned gamma:64
            !dir$ assume_aligned SN:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=1,n
                p0      = phi(i)
                g0      = gamma(i)
                s0      = compute_SN_r8(R,p0,g0)
                SN(i)   = s0
                p1      = phi(i+1)
                g1      = gamma(i+1)
                s1      = compute_SN_r8(R,p1,g1)
                SN(i+1) = s1
               
              end do
     end subroutine compute_SN_rolled_r8


     subroutine compute_SN_dispatch_r8(R,phi,gamma,n,SN,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: compute_SN_dispatch_r8
           real(kind=dp),                 intent(in) :: R
           real(kind=dp), dimension(1:n), intent(in) :: phi
           real(kind=dp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=dp), dimension(1:n), intent(out):: SN
           integer(kind=i4),              intent(in) :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                 call compute_SN_unroll_16x_r8(R,phi,gamma,n,SN)
              case (8)
                 call compute_SN_unroll_8x_r8(R,phi,gamma,n,SN)
              case (4)
                 call compute_SN_unroll_4x_r8(R,phi,gamma,n,SN)
              case (2)
                 call compute_SN_unroll_2x_r8(R,phi,gamma,n,SN)
              case (0)
                 call compute_SN_rolled_r8(R,phi,gamma,n,SN)
              case default
                 return
           end select
     end subroutine compute_SN_dispatch_r8



   

     ! Formula 2, p. 54
     ! расстояние SM от светящейся точки до ее изображения
     pure elemental function compute_SM_r4(R,phi,gamma) result(SM)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_r4
        !dir$ attributes forceinline :: compute_SM_r4
        real(kind=sp), intent(in) :: R
        real(kind=sp), intent(in) :: phi
        real(kind=sp), intent(in) :: gamma
        real(kind=sp) :: SM
        SM = 2.0_sp*compute_SN_r4(R,phi,gamma)
     end function compute_SM_r4


     subroutine compute_SM_unroll_16x_r4(R,phi,gamma,n,SM)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_SM_unroll_16x_r4
           !dir$ attributes forceinline ::  compute_SM_unroll_16x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_16x_r4
           real(kind=sp),                 intent(in) :: R
           real(kind=sp), dimension(1:n), intent(in) :: phi
           real(kind=sp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=sp), dimension(1:n), intent(out):: SM
           real(kind=sp), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
           real(kind=sp), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
           real(kind=sp), automatic :: g0,g1,g2,g3,g4,g5,g6,g7
           real(kind=sp), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
           real(kind=sp), automatic :: s0,s1,s2,s3,s4,s5,s6,s7
           real(kind=sp), automatic :: s8,s9,s10,s11,s12,s13,s14,s15
           integer(kind=i4) :: i,m,m1
           m = mod(n,16)
           if(m /= 0) then
              do i=1,m
                 s0 = compute_SM_r4(R,phi(i),gamma(i))
                 SN(i) = s0
              end do
              if(n<16) return
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
            do i=m1,n,16
                p0      = phi(i)
                g0      = gamma(i)
                s0      = compute_SM_r4(R,p0,g0)
                SN(i)   = s0
                p1      = phi(i+1)
                g1      = gamma(i+1)
                s1      = compute_SM_r4(R,p1,g1)
                SN(i+1) = s1
                p2      = phi(i+2)
                g2      = gamma(i+2)
                s2      = compute_SM_r4(R,p2,g2)
                SN(i+2) = s2
                p3      = phi(i+3)
                g3      = gamma(i+3)
                s3      = compute_SM_r4(R,p3,g3)
                SN(i+3) = s3
                p4      = phi(i+4)
                g4      = gamma(i+4)
                s4      = compute_SM_r4(R,p4,g4)
                SN(i+4) = s4
                p5      = phi(i+5)
                g5      = gamma(i+5)
                s5      = compute_SM_r4(R,p5,g5)
                SN(i+5) = s5
                p6      = phi(i+6)
                g6      = gamma(i+6)
                s6      = compute_SM_r4(R,p6,g6)
                SN(i+6) = s6
                p7      = phi(i+7)
                g7      = gamma(i+7)
                s7      = compute_SM_r4(R,p7,g7)
                SN(i+7) = s7
                p8      = phi(i+8)
                g8      = gamma(i+8)
                s8      = compute_SM_r4(R,p8,g8)
                SN(i+8) = s8
                p9      = phi(i+9)
                g9      = gamma(i+9)
                s9      = compute_SM_r4(R,p9,g9)
                SN(i+9) = s9
                p10     = phi(i+10)
                g10     = gamma(i+10)
                s10     = compute_SM_r4(R,p10,g10)
                SN(i+10)= s10
                p11     = phi(i+11)
                g11     = gamma(i+11)
                s11     = compute_SM_r4(R,p11,g11)
                SN(i+11)= s11
                p12     = phi(i+12)
                g12     = gamma(i+12)
                s12     = compute_SM_r4(R,p12,g12)
                SN(i+12)= s12
                p13     = phi(i+13)
                g13     = gamma(i+13)
                s13     = compute_SM_r4(R,p13,g13)
                SN(i+13)= s13
                p14     = phi(i+14)
                g14     = gamma(i+14)
                s14     = compute_SM_r4(R,p14,g14)
                SN(i+14)= s14
                p15     = phi(i+15)
                g15     = gamma(i+15)
                s15     = compute_SM_r4(R,p15,g15)
                SN(i+15)= s15
            end do
     end subroutine compute_SM_unroll_16x_r4


     subroutine compute_SM_unroll_8x_r4(R,phi,gamma,n,SM)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_SM_unroll_8x_r4
           !dir$ attributes forceinline ::  compute_SM_unroll_8x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_8x_r4
           real(kind=sp),                 intent(in) :: R
           real(kind=sp), dimension(1:n), intent(in) :: phi
           real(kind=sp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=sp), dimension(1:n), intent(out):: SM
           real(kind=sp), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
           real(kind=sp), automatic :: g0,g1,g2,g3,g4,g5,g6,g7
           real(kind=sp), automatic :: s0,s1,s2,s3,s4,s5,s6,s7
           integer(kind=i4) :: i,m,m1
           m = mod(n,8)
           if(m /= 0) then
              do i=1,m
                 s0 = compute_SM_r4(R,phi(i),gamma(i))
                 SN(i) = s0
              end do
              if(n<8) return
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
            do i=m1,n,8
                p0      = phi(i)
                g0      = gamma(i)
                s0      = compute_SM_r4(R,p0,g0)
                SN(i)   = s0
                p1      = phi(i+1)
                g1      = gamma(i+1)
                s1      = compute_SM_r4(R,p1,g1)
                SN(i+1) = s1
                p2      = phi(i+2)
                g2      = gamma(i+2)
                s2      = compute_SM_r4(R,p2,g2)
                SN(i+2) = s2
                p3      = phi(i+3)
                g3      = gamma(i+3)
                s3      = compute_SM_r4(R,p3,g3)
                SN(i+3) = s3
                p4      = phi(i+4)
                g4      = gamma(i+4)
                s4      = compute_SM_r4(R,p4,g4)
                SN(i+4) = s4
                p5      = phi(i+5)
                g5      = gamma(i+5)
                s5      = compute_SM_r4(R,p5,g5)
                SN(i+5) = s5
                p6      = phi(i+6)
                g6      = gamma(i+6)
                s6      = compute_SM_r4(R,p6,g6)
                SN(i+6) = s6
                p7      = phi(i+7)
                g7      = gamma(i+7)
                s7      = compute_SM_r4(R,p7,g7)
                SN(i+7) = s7
               end do
     end subroutine compute_SM_unroll_8x_r4


     subroutine compute_SM_unroll_4x_r4(R,phi,gamma,n,SM)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_SM_unroll_4x_r4
           !dir$ attributes forceinline ::  compute_SM_unroll_4x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_4x_r4
           real(kind=sp),                 intent(in) :: R
           real(kind=sp), dimension(1:n), intent(in) :: phi
           real(kind=sp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=sp), dimension(1:n), intent(out):: SM
           real(kind=sp), automatic :: p0,p1,p2,p3
           real(kind=sp), automatic :: g0,g1,g2,g3
           real(kind=sp), automatic :: s0,s1,s2,s3
           integer(kind=i4) :: i,m,m1
           m = mod(n,4)
           if(m /= 0) then
              do i=1,m
                 s0 = compute_SM_r4(R,phi(i),gamma(i))
                 SN(i) = s0
              end do
              if(n<4) return
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
            do i=m1,n,4
                p0      = phi(i)
                g0      = gamma(i)
                s0      = compute_SM_r4(R,p0,g0)
                SN(i)   = s0
                p1      = phi(i+1)
                g1      = gamma(i+1)
                s1      = compute_SM_r4(R,p1,g1)
                SN(i+1) = s1
                p2      = phi(i+2)
                g2      = gamma(i+2)
                s2      = compute_SM_r4(R,p2,g2)
                SN(i+2) = s2
                p3      = phi(i+3)
                g3      = gamma(i+3)
                s3      = compute_SM_r4(R,p3,g3)
                SN(i+3) = s3
             end do
     end subroutine compute_SM_unroll_4x_r4


     subroutine compute_SM_unroll_2x_r4(R,phi,gamma,n,SM)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_SM_unroll_2x_r4
           !dir$ attributes forceinline ::  compute_SM_unroll_2x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_2x_r4
           real(kind=sp),                 intent(in) :: R
           real(kind=sp), dimension(1:n), intent(in) :: phi
           real(kind=sp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=sp), dimension(1:n), intent(out):: SM
           real(kind=sp), automatic :: p0,p1
           real(kind=sp), automatic :: g0,g1
           real(kind=sp), automatic :: s0,s1
           integer(kind=i4) :: i,m,m1
           m = mod(n,2)
           if(m /= 0) then
              do i=1,m
                 s0 = compute_SM_r4(R,phi(i),gamma(i))
                 SN(i) = s0
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
            do i=m1,n,2
                p0      = phi(i)
                g0      = gamma(i)
                s0      = compute_SM_r4(R,p0,g0)
                SN(i)   = s0
                p1      = phi(i+1)
                g1      = gamma(i+1)
                s1      = compute_SM_r4(R,p1,g1)
                SN(i+1) = s1
            end do
     end subroutine compute_SM_unroll_2x_r4


     
     subroutine compute_SM_rolled_r4(R,phi,gamma,n,SM)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_SM_rolled_r4
           !dir$ attributes forceinline ::  compute_SM_rolled_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_rolled_r4
           real(kind=sp),                 intent(in) :: R
           real(kind=sp), dimension(1:n), intent(in) :: phi
           real(kind=sp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=sp), dimension(1:n), intent(out):: SM
           real(kind=sp), automatic :: p0
           real(kind=sp), automatic :: g0
           real(kind=sp), automatic :: s0
           integer(kind=i4) :: i
         
            !dir$ assume_aligned phi:64
            !dir$ assume_aligned gamma:64
            !dir$ assume_aligned SM:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=1,n
                p0      = phi(i)
                g0      = gamma(i)
                s0      = compute_SM_r4(R,p0,g0)
                SN(i)   = s0
               
            end do
     end subroutine compute_SM_rolled_r4


     subroutine compute_SM_dispatch_r4(R,phi,gamma,n,SM,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: compute_SM_dispatch_r4
           real(kind=dp),                 intent(in) :: R
           real(kind=dp), dimension(1:n), intent(in) :: phi
           real(kind=dp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=dp), dimension(1:n), intent(out):: SM
           integer(kind=i4),              intent(in) :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                 call compute_SM_unroll_16x_r4(R,phi,gamma,n,SN)
              case (8)
                 call compute_SM_unroll_8x_r4(R,phi,gamma,n,SN)
              case (4)
                 call compute_SM_unroll_4x_r4(R,phi,gamma,n,SN)
              case (2)
                 call compute_SM_unroll_2x_r4(R,phi,gamma,n,SN)
              case (0)
                 call compute_SM_rolled_r4(R,phi,gamma,n,SN)
              case default
                 return
           end select
     end subroutine compute_SM_dispatch_r4


     

 

     pure elemental function compute_SM_r8(R,phi,gamma) result(SM)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_r8
        !dir$ attributes forceinline :: compute_SM_r8
        real(kind=dp), intent(in) :: R
        real(kind=dp), intent(in) :: phi
        real(kind=dp), intent(in) :: gamma
        real(kind=dp) :: SM
        SM = 2.0_sp*compute_SN_r8(R,phi,gamma)
     end function compute_SM_r8


     subroutine compute_SM_unroll_16x_r8(R,phi,gamma,n,SM)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_SM_unroll_16x_r8
           !dir$ attributes forceinline ::  compute_SM_unroll_16x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_16x_r8
           real(kind=dp),                 intent(in) :: R
           real(kind=dp), dimension(1:n), intent(in) :: phi
           real(kind=dp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=dp), dimension(1:n), intent(out):: SM
           real(kind=dp), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
           real(kind=dp), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
           real(kind=dp), automatic :: g0,g1,g2,g3,g4,g5,g6,g7
           real(kind=dp), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
           real(kind=dp), automatic :: s0,s1,s2,s3,s4,s5,s6,s7
           real(kind=dp), automatic :: s8,s9,s10,s11,s12,s13,s14,s15
           integer(kind=i4) :: i,m,m1
           m = mod(n,16)
           if(m /= 0) then
              do i=1,m
                 s0 = compute_SM_r8(R,phi(i),gamma(i))
                 SN(i) = s0
              end do
              if(n<16) return
            end if
            m1 = m+1
            !dir$ assume_aligned phi:64
            !dir$ assume_aligned gamma:64
            !dir$ assume_aligned SM:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,16
                p0      = phi(i)
                g0      = gamma(i)
                s0      = compute_SM_r8(R,p0,g0)
                SN(i)   = s0
                p1      = phi(i+1)
                g1      = gamma(i+1)
                s1      = compute_SM_r8(R,p1,g1)
                SN(i+1) = s1
                p2      = phi(i+2)
                g2      = gamma(i+2)
                s2      = compute_SM_r8(R,p2,g2)
                SN(i+2) = s2
                p3      = phi(i+3)
                g3      = gamma(i+3)
                s3      = compute_SM_r8(R,p3,g3)
                SN(i+3) = s3
                p4      = phi(i+4)
                g4      = gamma(i+4)
                s4      = compute_SM_r8(R,p4,g4)
                SN(i+4) = s4
                p5      = phi(i+5)
                g5      = gamma(i+5)
                s5      = compute_SM_r8(R,p5,g5)
                SN(i+5) = s5
                p6      = phi(i+6)
                g6      = gamma(i+6)
                s6      = compute_SM_r8(R,p6,g6)
                SN(i+6) = s6
                p7      = phi(i+7)
                g7      = gamma(i+7)
                s7      = compute_SM_r8(R,p7,g7)
                SN(i+7) = s7
                p8      = phi(i+8)
                g8      = gamma(i+8)
                s8      = compute_SM_r8(R,p8,g8)
                SN(i+8) = s8
                p9      = phi(i+9)
                g9      = gamma(i+9)
                s9      = compute_SM_r8(R,p9,g9)
                SN(i+9) = s9
                p10     = phi(i+10)
                g10     = gamma(i+10)
                s10     = compute_SM_r8(R,p10,g10)
                SN(i+10)= s10
                p11     = phi(i+11)
                g11     = gamma(i+11)
                s11     = compute_SM_r8(R,p11,g11)
                SN(i+11)= s11
                p12     = phi(i+12)
                g12     = gamma(i+12)
                s12     = compute_SM_r8(R,p12,g12)
                SN(i+12)= s12
                p13     = phi(i+13)
                g13     = gamma(i+13)
                s13     = compute_SM_r8(R,p13,g13)
                SN(i+13)= s13
                p14     = phi(i+14)
                g14     = gamma(i+14)
                s14     = compute_SM_r8(R,p14,g14)
                SN(i+14)= s14
                p15     = phi(i+15)
                g15     = gamma(i+15)
                s15     = compute_SM_r8(R,p15,g15)
                SN(i+15)= s15
            end do
     end subroutine compute_SM_unroll_16x_r8


     subroutine compute_SM_unroll_8x_r8(R,phi,gamma,n,SM)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_SM_unroll_8x_r8
           !dir$ attributes forceinline ::  compute_SM_unroll_8x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_8x_r8
           real(kind=dp),                 intent(in) :: R
           real(kind=dp), dimension(1:n), intent(in) :: phi
           real(kind=dp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=dp), dimension(1:n), intent(out):: SM
           real(kind=dp), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
           real(kind=dp), automatic :: g0,g1,g2,g3,g4,g5,g6,g7
           real(kind=dp), automatic :: s0,s1,s2,s3,s4,s5,s6,s7
           integer(kind=i4) :: i,m,m1
           m = mod(n,8)
           if(m /= 0) then
              do i=1,m
                 s0 = compute_SM_r8(R,phi(i),gamma(i))
                 SN(i) = s0
              end do
              if(n<8) return
            end if
            m1 = m+1
            !dir$ assume_aligned phi:64
            !dir$ assume_aligned gamma:64
            !dir$ assume_aligned SM:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,8
                p0      = phi(i)
                g0      = gamma(i)
                s0      = compute_SM_r8(R,p0,g0)
                SN(i)   = s0
                p1      = phi(i+1)
                g1      = gamma(i+1)
                s1      = compute_SM_r8(R,p1,g1)
                SN(i+1) = s1
                p2      = phi(i+2)
                g2      = gamma(i+2)
                s2      = compute_SM_r8(R,p2,g2)
                SN(i+2) = s2
                p3      = phi(i+3)
                g3      = gamma(i+3)
                s3      = compute_SM_r8(R,p3,g3)
                SN(i+3) = s3
                p4      = phi(i+4)
                g4      = gamma(i+4)
                s4      = compute_SM_r8(R,p4,g4)
                SN(i+4) = s4
                p5      = phi(i+5)
                g5      = gamma(i+5)
                s5      = compute_SM_r8(R,p5,g5)
                SN(i+5) = s5
                p6      = phi(i+6)
                g6      = gamma(i+6)
                s6      = compute_SM_r8(R,p6,g6)
                SN(i+6) = s6
                p7      = phi(i+7)
                g7      = gamma(i+7)
                s7      = compute_SM_r8(R,p7,g7)
                SN(i+7) = s7
               end do
     end subroutine compute_SM_unroll_8x_r8


     subroutine compute_SM_unroll_4x_r8(R,phi,gamma,n,SM)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_SM_unroll_4x_r8
           !dir$ attributes forceinline ::  compute_SM_unroll_4x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_4x_r8
           real(kind=dp),                 intent(in) :: R
           real(kind=dp), dimension(1:n), intent(in) :: phi
           real(kind=dp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=dp), dimension(1:n), intent(out):: SM
           real(kind=dp), automatic :: p0,p1,p2,p3
           real(kind=dp), automatic :: g0,g1,g2,g3
           real(kind=dp), automatic :: s0,s1,s2,s3
           integer(kind=i4) :: i,m,m1
           m = mod(n,4)
           if(m /= 0) then
              do i=1,m
                 s0 = compute_SM_r8(R,phi(i),gamma(i))
                 SN(i) = s0
              end do
              if(n<4) return
            end if
            m1 = m+1
            !dir$ assume_aligned phi:64
            !dir$ assume_aligned gamma:64
            !dir$ assume_aligned SM:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,4
                p0      = phi(i)
                g0      = gamma(i)
                s0      = compute_SM_r8(R,p0,g0)
                SN(i)   = s0
                p1      = phi(i+1)
                g1      = gamma(i+1)
                s1      = compute_SM_r8(R,p1,g1)
                SN(i+1) = s1
                p2      = phi(i+2)
                g2      = gamma(i+2)
                s2      = compute_SM_r8(R,p2,g2)
                SN(i+2) = s2
                p3      = phi(i+3)
                g3      = gamma(i+3)
                s3      = compute_SM_r8(R,p3,g3)
                SN(i+3) = s3
             end do
     end subroutine compute_SM_unroll_4x_r8


     subroutine compute_SM_unroll_2x_r8(R,phi,gamma,n,SM)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_SM_unroll_2x_r8
           !dir$ attributes forceinline ::  compute_SM_unroll_2x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_2x_r8
           real(kind=dp),                 intent(in) :: R
           real(kind=dp), dimension(1:n), intent(in) :: phi
           real(kind=dp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=dp), dimension(1:n), intent(out):: SM
           real(kind=dp), automatic :: p0,p1
           real(kind=dp), automatic :: g0,g1
           real(kind=dp), automatic :: s0,s1
           integer(kind=i4) :: i,m,m1
           m = mod(n,2)
           if(m /= 0) then
              do i=1,m
                 s0 = compute_SM_r8(R,phi(i),gamma(i))
                 SN(i) = s0
              end do
              if(n<2) return
            end if
            m1 = m+1
            !dir$ assume_aligned phi:64
            !dir$ assume_aligned gamma:64
            !dir$ assume_aligned SM:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,2
                p0      = phi(i)
                g0      = gamma(i)
                s0      = compute_SM_r8(R,p0,g0)
                SN(i)   = s0
                p1      = phi(i+1)
                g1      = gamma(i+1)
                s1      = compute_SM_r8(R,p1,g1)
                SN(i+1) = s1
            end do
     end subroutine compute_SM_unroll_2x_r8


     
     subroutine compute_SM_rolled_r8(R,phi,gamma,n,SM)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_SM_rolled_r8
           !dir$ attributes forceinline ::  compute_SM_rolled_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_rolled_r8
           real(kind=dp),                 intent(in) :: R
           real(kind=dp), dimension(1:n), intent(in) :: phi
           real(kind=dp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=dp), dimension(1:n), intent(out):: SM
           real(kind=dp), automatic :: p0
           real(kind=dp), automatic :: g0
           real(kind=dp), automatic :: s0
           integer(kind=i4) :: i
         
            !dir$ assume_aligned phi:64
            !dir$ assume_aligned gamma:64
            !dir$ assume_aligned SM:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=1,n
                p0      = phi(i)
                g0      = gamma(i)
                s0      = compute_SM_r8(R,p0,g0)
                SN(i)   = s0
               
            end do
     end subroutine compute_SM_rolled_r8


     subroutine compute_SM_dispatch_r8(R,phi,gamma,n,SM,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: compute_SM_dispatch_r8
           real(kind=dp),                 intent(in) :: R
           real(kind=dp), dimension(1:n), intent(in) :: phi
           real(kind=dp), dimension(1:n), intent(in) :: gamma
           integer(kind=i4),              intent(in) :: n
           real(kind=dp), dimension(1:n), intent(out):: SM
           integer(kind=i4),              intent(in) :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                 call compute_SM_unroll_16x_r8(R,phi,gamma,n,SN)
              case (8)
                 call compute_SM_unroll_8x_r8(R,phi,gamma,n,SN)
              case (4)
                 call compute_SM_unroll_4x_r8(R,phi,gamma,n,SN)
              case (2)
                 call compute_SM_unroll_2x_r8(R,phi,gamma,n,SN)
              case (0)
                 call compute_SM_rolled_r8(R,phi,gamma,n,SN)
              case default
                 return
           end select
     end subroutine compute_SM_dispatch_r8


     

     
   


     !Сканирующее зеркало для обеспечения осмотра всего поля
     !обзора ф необходимо повернуть на угол, обеспечивающий 
     !совмещение края изображения источника излучения с отверстием 
     !диафрагмы а, находящимся в центре поля. Для этого необходимо 
     !повернуть изображение светящейся точки S на угол ф/2
     ! Formula 1, p. 56
     pure elemental function ratio_FH_r4(psi,phi) result(FH)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_r4
        !dir$ attributes forceinline :: ratio_FH_r4
        real(kind=sp),  intent(in) :: psi
        real(kind=sp),  intent(in) :: phi
        real(kind=sp) :: FH
        real(kind=sp), automatic :: hpsi,hphi
        hpsi = 0.5_sp*psi
        hphi = 0.5_sp*phi
        FH   = tan(hpsi)/tan(hphi)
     end function ratio_FH_r4


     pure elemental function ratio_FH_r8(psi,phi) result(FH)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_r8
        !dir$ attributes forceinline :: ratio_FH_r8
        real(kind=dp),  intent(in) :: psi
        real(kind=dp),  intent(in) :: phi
        real(kind=dp) :: FH
        real(kind=dp), automatic :: hpsi,hphi
        hpsi = 0.5_dp*psi
        hphi = 0.5_dp*phi
        FH   = tan(hpsi)/tan(hphi)
     end function ratio_FH_r8


   


     ! следовательно, угол установки сканирующего зеркала
     ! Formula 4, p. 56
     pure elemental function scan_mirror_ang_r4(gam0,psi,phi,dir) result(gamma)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_r4
        !dir$ attributes forceinline :: scan_mirror_ang_r4
        real(kind=sp),  intent(in) :: gam0
        real(kind=sp),  intent(in) :: psi
        real(kind=sp),  intent(in) :: phi
        character(len=3), intent(in) :: dir
        real(kind=sp) :: gamma
        real(kind=sp), automatic :: t0
        if(dir=="pos") then
           t0 = gam0+0.5_sp*phi*0.5_sp
           gamma = t0*ratio_FH_r4(psi,phi) 
        else if(dir=="neg") then
           t0 = gam0-0.5_sp*phi*0.5_sp
           gamma = t0*ratio_FH_r4(psi,phi)
        end if
     end function scan_mirror_ang_r4


     pure elemental function scan_mirror_ang_r8(gam0,psi,phi,dir) result(gamma)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_r8
        !dir$ attributes forceinline :: scan_mirror_ang_r8
        real(kind=dp),  intent(in) :: gam0
        real(kind=dp),  intent(in) :: psi
        real(kind=dp),  intent(in) :: phi
        character(len=3), intent(in) :: dir
        real(kind=dp) :: gamma
        real(kind=dp), automatic :: t0
        if(dir=="pos") then
           t0 = gam0+0.5_dp*phi*0.5_dp
           gamma = t0*ratio_FH_r8(psi,phi) 
        else if(dir=="neg") then
           t0 = gam0-0.5_dp*phi*0.5_dp
           gamma = t0*ratio_FH_r8(psi,phi)
        end if
     end function scan_mirror_ang_r8


    

     ! Maximum size of (verical) diameter of scanning mirror.
     ! Formula 2, page. 56, part: 1.3
     ! Anax = [h tg (6/2) + do6/2] [2 cos y' + sin yf {tg (у' + 6/2) +
     !+ tg(Y'-6/2)}].
     pure function compute_Dmax_r4(h,delta,d_ob,gamma) result(Dmax)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_Dmax_r4
        !dir$ attributes forceinline :: compute_Dmax_r4
        real(kind=sp),  intent(in) :: h
        real(kind=sp),  intent(in) :: delta
        real(kind=sp),  intent(in) :: d_ob
        real(kind=sp),  intent(in) :: gamma
        real(kind=sp) :: Dmax
        real(kind=sp), automatic :: delta2,d_ob2,cosg,sing,t0,t1,t2,tant0,tant1
        real(kind=sp), automatic :: t3,t4,t5
        delta2 = 0.5_sp*delta
        d_ob2  = 0.5_sp*d_ob
        cosg   = cos(gamma)
        if(delta<=gamma) then
           t0  = h*delta+d_ob
           Dmax= t0/cosg
        end if
        t0     = gamma+delta2
        t1     = gamma-delta2
        sing   = sin(gamma)
        tant1  = tan(t0)
        tant2  = tan(t1)
        t3     = h*tan(delta2)+d_ob2
        t4     = 2.0_sp*cosg+sing
        t5     = tant1+tant2
        Dmax   = t3*t4*t5
     end function compute_Dmax_r4


     pure function compute_Dmax_r8(h,delta,d_ob,gamma) result(Dmax)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_Dmax_r8
        !dir$ attributes forceinline :: compute_Dmax_r8
        real(kind=dp),  intent(in) :: h
        real(kind=dp),  intent(in) :: delta
        real(kind=dp),  intent(in) :: d_ob
        real(kind=dp),  intent(in) :: gamma
        real(kind=dp) :: Dmax
        real(kind=dp), automatic :: delta2,d_ob2,cosg,sing,t0,t1,t2,tant0,tant1
        real(kind=dp), automatic :: t3,t4,t5
        delta2 = 0.5_dp*delta
        d_ob2  = 0.5_dp*d_ob
        cosg   = cos(gamma)
        if(delta<=gamma) then
           t0  = h*delta+d_ob
           Dmax= t0/cosg
        end if 
        t0     = gamma+delta2
        t1     = gamma-delta2
        sing   = sin(gamma)
        tant1  = tan(t0)
        tant2  = tan(t1)
        t3     = h*tan(delta2)+d_ob2
        t4     = 2.0_dp*cosg+sing
        t5     = tant1+tant2
        Dmax   = t3*t4*t5
     end function compute_Dmax_r8


     ! Размер зеркала в направлении, перпендикулярном плоскости
     ! чертежа, приблизительно равен
     ! Formula 2, p. 58
     pure function compute_Dmin_r4(h,delta,d_ob) result(Dmin)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: compute_Dmin_r4
          !dir$ attributes forceinline :: compute_Dmin_r4
          real(kind=sp),  intent(in) :: h
          real(kind=sp),  intent(in) :: delta
          real(kind=sp),  intent(in) :: d_ob
          real(kind=sp) :: Dmin
          Dmin = h*delta+d_ob
     end function compute_Dmin_r4


     pure function compute_Dmin_r8(h,delta,d_ob) result(Dmin)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: compute_Dmin_r8
          !dir$ attributes forceinline :: compute_Dmin_r8
          real(kind=dp),  intent(in) :: h
          real(kind=dp),  intent(in) :: delta
          real(kind=dp),  intent(in) :: d_ob
          real(kind=dp) :: Dmin
          Dmin = h*delta+d_ob
     end function compute_Dmin_r8


     !Если зеркало осуществляет сканирование в пространстве
     !изображений его размеры
     ! Formula 7, p. 58
     pure function Dmax_imag_scan_r4(H,F,B,d_ob,gamma, &
                                     psi,phi,d) result(Dmax)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: Dmax_imag_scan_r4
          !dir$ attributes forceinline :: Dmax_imag_scan_r4
          real(kind=sp),  intent(in) :: H
          real(kind=sp),  intent(in) :: F
          real(kind=sp),  intent(in) :: B
          real(kind=sp),  intent(in) :: d_ob
          real(kind=sp),  intent(in) :: gamma
          real(kind=sp),  intent(in) :: psi
          real(kind=sp),  intent(in) :: phi
          real(kind=sp),  intent(in) :: d
          real(kind=sp) :: Dmax
          real(kind=sp), automatic :: t0,t1,t2,t3
          real(kind=sp), automatic :: cosg,sing,tanp1,tanp2,psi2,phi2
          psi2  = 0.5_sp*psi
          if(psi2<=gamma .and. &
             B<=d) then
             phi2 = 0.5_sp*phi
             t0   = (F+F)*tan(phi2)
             t1   = (H/F)*d_ob
             t2   = sin(gamma)
             Dmax = (t0+t1)*t2
          end if
          t0    = (H/F)*(d_ob-B)+B
          cosg  = cos(gamma)
          tanp1 = gamma+psi2
          tanp2 = gamma-psi2
          sing  = sin(gamma)
          t1    = 2.0_sp*cosg+sing
          t2    = tan(tanp1)+tan(tanp2)
          t3    = 0.5_sp*t1*t2
          Dmax  = t0*t3
     end function Dmax_imag_scan_r4


     pure function Dmax_imag_scan_r8(H,F,B,d_ob,gamma, &
                                     psi,phi,d) result(Dmax)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: Dmax_imag_scan_r8
          !dir$ attributes forceinline :: Dmax_imag_scan_r8
          real(kind=dp),  intent(in) :: H
          real(kind=dp),  intent(in) :: F
          real(kind=dp),  intent(in) :: B
          real(kind=dp),  intent(in) :: d_ob
          real(kind=dp),  intent(in) :: gamma
          real(kind=dp),  intent(in) :: psi
          real(kind=dp),  intent(in) :: phi
          real(kind=dp),  intent(in) :: d
          real(kind=dp) :: Dmax
          real(kind=dp), automatic :: t0,t1,t2,t3
          real(kind=dp), automatic :: cosg,sing,tanp1,tanp2,psi2,phi2
          psi2  = 0.5_dp*psi
          if(psi2<=gamma .and. &
             B<=d) then
             phi2 = 0.5_dp*phi
             t0   = (F+F)*tan(phi2)
             t1   = (H/F)*d_ob
             t2   = sin(gamma)
             Dmax = (t0+t1)*t2
          end if
          t0    = (H/F)*(d_ob-B)+B
          cosg  = cos(gamma)
          tanp1 = gamma+psi2
          tanp2 = gamma-psi2
          sing  = sin(gamma)
          t1    = 2.0_dp*cosg+sing
          t2    = tan(tanp1)+tan(tanp2)
          t3    = 0.5_dp*t1*t2
          Dmax  = t0*t3
     end function Dmax_imag_scan_r8


     pure function Dmin_imag_scan_r4(H,F,d_ob,B) result(Dmin)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: Dmin_imag_scan_r4
          !dir$ attributes forceinline :: Dmin_imag_scan_r4
          real(kind=sp),     intent(in) :: H
          real(kind=sp),     intent(in) :: F
          real(kind=sp),     intent(in) :: d_ob
          real(kind=sp),     intent(in) :: B
          real(kind=sp) :: Dmin
          real(kind=sp), automatic :: t0,t1
          t0   = H/F
          t1   = (d_ob-B)+B
          Dmin = t0*t1 
     end function Dmin_imag_scan_r4


     pure function Dmin_imag_scan_r8(H,F,d_ob,B) result(Dmin)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: Dmin_imag_scan_r8
          !dir$ attributes forceinline :: Dmin_imag_scan_r8
          real(kind=dp),     intent(in) :: H
          real(kind=dp),     intent(in) :: F
          real(kind=dp),     intent(in) :: d_ob
          real(kind=dp),     intent(in) :: B
          real(kind=dp) :: Dmin
          real(kind=dp), automatic :: t0,t1
          t0   = H/F
          t1   = (d_ob-B)+B
          Dmin = t0*t1 
     end function Dmin_imag_scan_r8


    !величина расфокусировки
    !Formula 1, p. 59
    pure elemental function defocus_cof_r4(l2,alpha,O,inf) result(dc)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: defocus_cof_r4
          !dir$ attributes forceinline :: defocus_cof_r4
          real(kind=sp),    intent(in) :: l2
          real(kind=sp),    intent(in) :: alpha
          real(kind=sp),    intent(in) :: O
          logical(kind=i4), intent(in) :: inf
          real(kind=sp) :: df
          real(kind=sp), automatic :: cos2a,icos
          cos2a = cos(alpha+alpha)
          icos  = 1.0_sp/cos2a
          if(inf) then
             df    = l2/(icos-1.0_sp)*O
          else
             df    = l2/(icos-1.0_sp)
          end if
    end function defocus_cof_r4


    pure elemental function defocus_cof_r8(l2,alpha,O,inf) result(dc)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: defocus_cof_r8
          !dir$ attributes forceinline :: defocus_cof_r8
          real(kind=dp),    intent(in) :: l2
          real(kind=dp),    intent(in) :: alpha
          real(kind=dp),    intent(in) :: O
          logical(kind=i4), intent(in) :: inf
          real(kind=dp) :: df
          real(kind=dp), automatic :: cos2a,icos
          cos2a = cos(alpha+alpha)
          icos  = 1.0_dp/cos2a
          if(inf) then
             df    = l2/(icos-1.0_dp)*O
          else
             df    = l2/(icos-1.0_dp)
          end if
    end function defocus_cof_r8


   

    ! Диаметр кружка рассеяния р
    ! Formula 3, p.59
    pure elemental function circle_dispersion_r4(d,l1,l2,alpha,O,inf) result(rho)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: circle_dispersion_r4
          !dir$ attributes forceinline :: circle_dispersion_r4
          real(kind=sp),    intent(in) :: d
          real(kind=sp),    intent(in) :: l1
          real(kind=sp),    intent(in) :: l2
          real(kind=sp),    intent(in) :: alpha
          real(kind=sp),    intent(in) :: O
          logical(kind=i4), intent(in) :: inf
          real(kind=sp) :: rho
          real(kind=sp), automatic :: t0,t1
          t0  = d/(l1+l2)
          t1  = defocus_cof_r4(l2,alpha,O,inf)
          rho = t0*t1
    end function circle_dispersion_r4


    pure elemental function circle_dispersion_r8(d,l1,l2,alpha,O,inf) result(rho)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: circle_dispersion_r8
          !dir$ attributes forceinline :: circle_dispersion_r8
          real(kind=dp),    intent(in) :: d
          real(kind=dp),    intent(in) :: l1
          real(kind=dp),    intent(in) :: l2
          real(kind=dp),    intent(in) :: alpha
          real(kind=dp),    intent(in) :: O
          logical(kind=i4), intent(in) :: inf
          real(kind=dp) :: rho
          real(kind=dp), automatic :: t0,t1
          t0  = d/(l1+l2)
          t1  = defocus_cof_r8(l2,alpha,O,inf)
          rho = t0*t1
    end function circle_dispersion_r8


  

      
     !Formula 2, p. 59
     pure elemental function circ_dispers_diam_r4(l1,l2,alpha,O,inf) result(ratio)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: circ_dispers_diam_r4
         !dir$ attributes forceinline :: circ_dispers_diam_r4
         real(kind=sp),    intent(in) :: l1
         real(kind=sp),    intent(in) :: l2
         real(kind=sp),    intent(in) :: alpha
         real(kind=sp),    intent(in) :: O
         logical(kind=i4), intent(in) :: inf
         real(kind=sp) :: ratio
         real(kind=sp), automatic :: t0,t1
         t0    = l1+l2
         t1    = defocus_cos_r4(l2,alpha,O,inf)
         ratio = t1/t0
     end function circ_dispers_diam_r4


     pure elemental function circ_dispers_diam_r4(l1,l2,alpha,O,inf) result(ratio)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: circ_dispers_diam_r8
         !dir$ attributes forceinline :: circ_dispers_diam_r8
         real(kind=dp),    intent(in) :: l1
         real(kind=dp),    intent(in) :: l2
         real(kind=dp),    intent(in) :: alpha
         real(kind=dp),    intent(in) :: O
         logical(kind=i4), intent(in) :: inf
         real(kind=dp) :: ratio
         real(kind=dp), automatic :: t0,t1
         t0    = l1+l2
         t1    = defocus_cos_r4(l2,alpha,O,inf)
         ratio = t1/t0
     end function circ_dispers_diam_r4


    
      
       
     pure elemental function defocus_small_ang_r4(O,l2,alpha) result(rho)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: defocus_small_ang_r4
         !dir$ attributes forceinline :: defocus_small_ang_r4
         use mod_fpcompare, only : Compare_Float
         real(kind=sp),   intent(in) :: O
         real(kind=sp),   intent(in) :: L2
         real(kind=sp),   intent(in) :: alpha
         real(kind=sp) :: rho
         real(kind=sp), automatic :: t0,t1,t2,alpha2
         alpha2 = alpha+alpha
         t0     = cos(alpha2)
         t1     = 1.0_sp-alpha2*alpha2*0.5_sp
         if(Compare_FLoat(t0,t1)) then
            t2  = l2*0.5_sp
            rho = O*t2*alpha2*alpha2
         end if
         rho = tiny(1.0_sp)
      end function defocus_small_ang_r4


      pure elemental function defocus_small_ang_r8(O,l2,alpha) result(rho)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: defocus_small_ang_r8
         !dir$ attributes forceinline :: defocus_small_ang_r8
         use mod_fpcompare, only : Compare_Float
         real(kind=dp),   intent(in) :: O
         real(kind=dp),   intent(in) :: L2
         real(kind=dp),   intent(in) :: alpha
         real(kind=dp) :: rho
         real(kind=dp), automatic :: t0,t1,t2,alpha2
         alpha2 = alpha+alpha
         t0     = cos(alpha2)
         t1     = 1.0_dp-alpha2*alpha2*0.5_dp
         if(Compare_FLoat(t0,t1)) then
            t2  = l2*0.5_dp
            rho = O*t2*alpha2*alpha2
         end if
         rho = tiny(1.0_dp)
      end function defocus_small_ang_r8


      pure elemental function traj_scan_dxdt_r4(dx,dt) result(dxdt)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: traj_scan_dxdt_r4
         !dir$ attributes forceinline :: traj_scan_dxdt_r4
         real(kind=sp), dimension(0:1), intent(in) :: dx
         real(kind=sp), dimension(0:1), intent(in) :: dt
         real(kind=sp) :: dxdt
         dxdt = dx(1)-dx(0)/(dt(1)-dt(0))
      end function traj_scan_dxdt_r4


      pure elemental function traj_scan_dxdt_r8(dx,dt) result(dxdt)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: traj_scan_dxdt_r8
         !dir$ attributes forceinline :: traj_scan_dxdt_r8
         real(kind=dp), dimension(0:1), intent(in) :: dx
         real(kind=dp), dimension(0:1), intent(in) :: dt
         real(kind=dp) :: dxdt
         dxdt = dx(1)-dx(0)/(dt(1)-dt(0))
      end function traj_scan_dxdt_r8


      pure elemental function traj_scan_dydt_r4(dy,dt) result(dydt)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: traj_scan_dydt_r4
         !dir$ attributes forceinline :: traj_scan_dydt_r4
         real(kind=sp), dimension(0:1), intent(in) :: dy
         real(kind=sp), dimension(0:1), intent(in) :: dt
         real(kind=sp) :: dydt
         dxdt = dy(1)-dy(0)/(dt(1)-dt(0))
      end function traj_scan_dydt_r4

       
      pure elemental function traj_scan_dydt_r8(dy,dt) result(dydt)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: traj_scan_dydt_r8
         !dir$ attributes forceinline :: traj_scan_dydt_r8
         real(kind=dp), dimension(0:1), intent(in) :: dx=y
         real(kind=dp), dimension(0:1), intent(in) :: dt
         real(kind=dp) :: dydt
         dxdt = dy(1)-dy(0)/(dt(1)-dt(0))
      end function traj_scan_dydt_r8


      ! СКАНИРОВАНИЕ ЗЕРКАЛОМ, ВРАЩАЮЩИМСЯ
      ! ВОКРУГ ОСИ, НЕПЕРПЕНДИКУЛЯРНОЙ К НЕМУ
      ! Formula 1, p. 100
      pure elemental function fov_x_axis_r4(H,delta,gamma) result(ax)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: fov_x_axis_r4
         !dir$ attributes forceinline :: fov_x_axis_r4
         real(kind=sp),  intent(in) :: H
         real(kind=sp),  intent(in) :: delta
         real(kind=sp),  intent(in) :: gamma
         real(kind=sp) :: ax
         real(kind=sp), automatic :: gamm2,tdel
         gamm2 = 0.5_sp*gamma
         tdel  = tan(delta)
         ax    = H*tdel/cos(gamm2)
      end function fov_x_axis_r4


      pure elemental function fov_x_axis_r8(H,delta,gamma) result(ax)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: fov_x_axis_r8
         !dir$ attributes forceinline :: fov_x_axis_r8
         real(kind=dp),  intent(in) :: H
         real(kind=dp),  intent(in) :: delta
         real(kind=dp),  intent(in) :: gamma
         real(kind=dp) :: ax
         real(kind=dp), automatic :: gamm2,tdel
         gamm2 = 0.5_dp*gamma
         tdel  = tan(delta)
         ax    = H*tdel/cos(gamm2)
      end function fov_x_axis_r8


      pure elemental function fov_y_axis_r4(H,delta,gamma) result(ay)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: fov_y_axis_r4
         !dir$ attributes forceinline :: fov_y_axis_r4
         real(kind=sp),  intent(in) :: H
         real(kind=sp),  intent(in) :: delta
         real(kind=sp),  intent(in) :: gamma
         real(kind=sp) :: ay
         real(kind=sp) :: ax,t0
         t0 = 0.5_sp*gamma
         ax = fov_x_axis_r4(H,delta,gamma)
         ay = t0*ax
      end function fov_y_axis_r4


      pure elemental function fov_y_axis_r8(H,delta,gamma) result(ay)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: fov_y_axis_r8
         !dir$ attributes forceinline :: fov_y_axis_r8
         real(kind=dp),  intent(in) :: H
         real(kind=dp),  intent(in) :: delta
         real(kind=dp),  intent(in) :: gamma
         real(kind=dp) :: ay
         real(kind=dp) :: ax,t0
         t0 = 0.5_dp*gamma
         ax = fov_x_axis_r8(H,delta,gamma)
         ay = t0*ax
      end function fov_y_axis_r8


   


     
       

     


      !Если рабочая зона сканирования ограничена углом G, то
      !ширина захвата
      !Formula 3, p. 100
     pure elemental function scan_width_r4(H,gamma,theta) result(B)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_r4
         !dir$ attributes forceinline :: scan_width_r4
         real(kind=sp),   intent(in) :: H
         real(kind=sp),   intent(in) :: gamma
         real(kind=sp),   intent(in) :: theta
         real(kind=sp) :: B
         real(kind=sp), automatic :: gam2,th2,t0,t1
         gam2  = 0.5_sp*gamma
         th2   = 0.5_sp*theta
         t0    = tan(gam2)
         t1    = sin(th2)
         B     = (H+H)*t0*t1
      end function scan_width_r4


      pure elemental function scan_width_r8(H,gamma,theta) result(B)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_r8
         !dir$ attributes forceinline :: scan_width_r8
         real(kind=dp),   intent(in) :: H
         real(kind=dp),   intent(in) :: gamma
         real(kind=dp),   intent(in) :: theta
         real(kind=dp) :: B
         real(kind=dp), automatic :: gam2,th2,t0,t1
         gam2  = 0.5_dp*gamma
         th2   = 0.5_dp*theta
         t0    = tan(gam2)
         t1    = sin(th2)
         B     = (H+H)*t0*t1
      end function scan_width_r8


   


      !Плоскопараллельная пластинка, установленная за 
      !объективом, изменяет ход лучей таким образом, что изображение
      ! светящейся точки отодвигается и его положение зависит от угла у
      !между оптической осью и нормалью N к поверхности пластинки
      ! Formula 7,8 p. 106
      pure elemental function refract_shift_r4(i1,delta,alfa,gamma,n) result(l)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: refract_shift_r4
         !dir$ attributes forceinline :: refract_shift_r4
         use mod_fpcompare, only : Compare_Float
         real(kind=sp),   intent(in) :: i1
         real(kind=sp),   intent(in) :: delta
         real(kind=sp),   intent(in) :: alfa
         real(kind=sp),   intent(in) :: gamma
         real(kind=sp),   intent(in) :: n
         real(kind=sp) :: l
         real(kind=sp), automatic :: ag,num,den,sin2,sag,t0,t1
         ag  = alfa-gamma
         if(Compare_Float(i1,ag)) then
            sag  = sin(ag)
            t0   = delta*sag
            sin2 = sag*sag
            num  = 1.0_sp-sag
            den  = n*n-sag
            t1   = 1.0_sp-sqrt(num/den)
            l    = t0*t2
         else if(alfa==0.0_sp) then
            sag  = sin(gamma)
            t0   = -delta*sag
            sin2 = sag*sag
            num  = 1.0_sp-sin2
            den  = n*n-sin2
            t1   = 1.0_sp-sqrt(num/den)
            l    = t0*t1
         else
            sag  = sin(i1)
            t0   = delta*sag
            sin2 = sag*sag
            num  = 1.0_sp-sin2
            den  = n*n-sin2
            t1   = 1.0_sp-sqrt(num/den)
            l    = t0*t1
         end if
      end function refract_shift_r4


      pure elemental function refract_shift_r8(i1,delta,alfa,gamma,n) result(l)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: refract_shift_r8
         !dir$ attributes forceinline :: refract_shift_r8
         use mod_fpcompare, only : Compare_Float
         real(kind=dp),   intent(in) :: i1
         real(kind=dp),   intent(in) :: delta
         real(kind=dp),   intent(in) :: alfa
         real(kind=dp),   intent(in) :: gamma
         real(kind=dp),   intent(in) :: n
         real(kind=dp) :: l
         real(kind=dp), automatic :: ag,num,den,sin2,sag,t0,t1
         ag  = alfa-gamma
         if(Compare_Float(i1,ag)) then
            sag  = sin(ag)
            t0   = delta*sag
            sin2 = sag*sag
            num  = 1.0_dp-sag
            den  = n*n-sag
            t1   = 1.0_dp-sqrt(num/den)
            l    = t0*t2
         else if(alfa==0.0_dp) then
            sag  = sin(gamma)
            t0   = -delta*sag
            sin2 = sag*sag
            num  = 1.0_dp-sin2
            den  = n*n-sin2
            t1   = 1.0_dp-sqrt(num/den)
            l    = t0*t1
         else
            sag  = sin(i1)
            t0   = delta*sag
            sin2 = sag*sag
            num  = 1.0_dp-sin2
            den  = n*n-sin2
            t1   = 1.0_dp-sqrt(num/den)
            l    = t0*t1
         end if
     end function refract_shift_r8

       
    

          
       

      !Formula 1, p. 108
      subroutine project_xy_axis_r4(l,alpha,xl,yl)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: project_xy_axis_r4
         !dir$ attributes forceinline :: project_xy_axis_r4
         real(kind=sp),  intent(in)  :: l
         real(kind=sp),  intent(in)  :: alpha
         real(kind=sp),  intent(out) :: xl
         real(kind=sp),  intent(out) :: yl
         real(kind=sp), automatic :: absl
         absl = abs(l)
         xl = absl*cos(alpha)
         yl = absl*sin(alpha)
      end subroutine project_xy_axis_r4
 

      subroutine project_xy_axis_r8(l,alpha,xl,yl)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: project_xy_axis_r8
         !dir$ attributes forceinline :: project_xy_axis_r8
         real(kind=dp),  intent(in)  :: l
         real(kind=dp),  intent(in)  :: alpha
         real(kind=dp),  intent(out) :: xl
         real(kind=dp),  intent(out) :: yl
         real(kind=dp), automatic :: absl
         absl = abs(l)
         xl = absl*cos(alpha)
         yl = absl*sin(alpha)
     end subroutine project_xy_axis_r8


    

       
      !Величину смещения луча s вдоль перпендикуляра к 
      !поверхности пластинки
      ! Formula 2, p. 108
      pure elemental function s_shift_r4(l,alpha,gamma) result(s)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 ::s_shift_r4
         !dir$ attributes forceinline :: s_shift_r4
         real(kind=sp),   intent(in) :: l
         real(kind=sp),   intent(in) :: alpha
         real(kind=sp),   intent(in) :: gamma
         real(kind=sp) :: s
         real(kind=sp), automatic :: ag,sag
         ag = alpha-gamma
         sag= sin(ag)
         s  = l/sag
      end function s_shift_r4


      pure elemental function s_shift_r8(l,alpha,gamma) result(s)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 ::s_shift_r8
         !dir$ attributes forceinline :: s_shift_r8
         real(kind=dp),   intent(in) :: l
         real(kind=dp),   intent(in) :: alpha
         real(kind=dp),   intent(in) :: gamma
         real(kind=dp) :: s
         real(kind=dp), automatic :: ag,sag
         ag = alpha-gamma
         sag= sin(ag)
         s  = l/sag
      end function s_shift_r8


    

      ! Проекции s на оси координат равны
      ! Formula 4, p. 108
      subroutine project_s_xy_r4(s,gamma,xs,ys)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: project_s_xy_r4
         !dir$ attributes forceinline :: project_s_xy_r4
         real(kind=sp),   intent(in)  :: s
         real(kind=sp),   intent(in)  :: gamma
         real(kind=sp),   intent(out) :: xs
         real(kind=sp),   intent(in)  :: ys
         xs = s*cos(gamma)
         ys = s*sin(gamma)
      end subroutine project_s_xy_r4


      subroutine project_s_xy_r8(s,gamma,xs,ys)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: project_s_xy_r8
         !dir$ attributes forceinline :: project_s_xy_r8
         real(kind=dp),   intent(in)  :: s
         real(kind=dp),   intent(in)  :: gamma
         real(kind=dp),   intent(out) :: xs
         real(kind=dp),   intent(in)  :: ys
         xs = s*cos(gamma)
         ys = s*sin(gamma)
      end subroutine project_s_xy_r8


    

      ! что расстояния от начала координат О до точек
      ! пересечения лучей, образующих с горизонталью угла ±а, с 
      ! перпендикуляром к пластинке
      ! Formula 1, p. 110
      pure elemental function ray_intercept_pa_r4(delta,alpha,gamma,n) result(sp)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: ray_intercept_pa_r4
         !dir$ attributes forceinline :: ray_intercept_pa_r4
         real(kind=sp),   intent(in) :: delta
         real(kind=sp),   intent(in) :: alpha
         real(kind=sp),   intent(in) :: gamma
         real(kind=sp),   intent(in) :: n
         real(kind=sp) :: sp
         real(kind=sp), automatic :: ag,num,den,n2,sag,sin2
         ag  = abs(alpha)-gamma
         n2  = n*n
         num = cos(ag)
         sag = sin(ag)
         sin2= sag*sag
         den = sqrt(n2-sin2)
         sp  = delta*1.0_sp-(num/den)
      end function ray_intercept_pa_r4


      pure elemental function ray_intercept_pa_r8(delta,alpha,gamma,n) result(sp)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: ray_intercept_pa_r8
         !dir$ attributes forceinline :: ray_intercept_pa_r8
         real(kind=dp),   intent(in) :: delta
         real(kind=dp),   intent(in) :: alpha
         real(kind=dp),   intent(in) :: gamma
         real(kind=dp),   intent(in) :: n
         real(kind=dp) :: sp
         real(kind=dp), automatic :: ag,num,den,n2,sag,sin2
         ag  = abs(alpha)-gamma
         n2  = n*n
         num = cos(ag)
         sag = sin(ag)
         sin2= sag*sag
         den = sqrt(n2-sin2)
         sp  = delta*1.0_dp-(num/den)
      end function ray_intercept_pa_r8


    


      pure elemental function ray_intercept_na_r4(delta,alpha,gamma,n) result(sn)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: ray_intercept_na_r4
         !dir$ attributes forceinline :: ray_intercept_na_r4
         real(kind=sp),   intent(in) :: delta
         real(kind=sp),   intent(in) :: alpha
         real(kind=sp),   intent(in) :: gamma
         real(kind=sp),   intent(in) :: n
         real(kind=sp) :: sn
         real(kind=sp), automatic :: ag,num,den,n2,sag,sin2
         ag  = abs(alpha)+gamma
         n2  = n*n
         num = cos(ag)
         sag = sin(ag)
         sin2= sag*sag
         den = sqrt(n2-sin2)
         sn  = delta*1.0_sp-(num/den)
      end function ray_intercept_na_r4


      pure elemental function ray_intercept_na_r8(delta,alpha,gamma,n) result(sn)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: ray_intercept_na_r8
         !dir$ attributes forceinline :: ray_intercept_na_r8
         real(kind=dp),   intent(in) :: delta
         real(kind=dp),   intent(in) :: alpha
         real(kind=dp),   intent(in) :: gamma
         real(kind=dp),   intent(in) :: n
         real(kind=dp) :: sn
         real(kind=dp), automatic :: ag,num,den,n2,sag,sin2
         ag  = abs(alpha)+gamma
         n2  = n*n
         num = cos(ag)
         sag = sin(ag)
         sin2= sag*sag
         den = sqrt(n2-sin2)
         sn  = delta*1.0_dp-(num/den)
      end function ray_intercept_na_r8


    


       ! Formula 3, p. 110
       pure function ray_diff_r4(delta,alpha,gamma,n,u) result(ds)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_diff_r4
           !dir$ attributes forceinline :: ray_diff_r4
           real(kind=sp),   intent(in) :: delta
           real(kind=sp),   intent(in) :: alpha
           real(kind=sp),   intent(in) :: gamma
           real(kind=sp),   intent(in) :: n
           real(kind=sp),   intent(in) :: u
           real(kind=sp) :: ds
           real(kind=sp), automatic :: t0,t1,u2,u2g,su2,sg,t2,n2,t3,t4,t5
           n   = n*n
           u2  = u*0.5_sp
           u2g = u2-gamma
           t2  = sin(u2g)
           su2 = t2*t2
           if(n2>=su2) then
              t3 = (-2.0_sp*delta)/n
              t4 = sin(u2)
              t5 = sin(gamma)
              ds = t3*t4*t5
           else
              t0  = ray_intercept_pa_r4(delta,alpha,gamma,n)
              t1  = ray_intercept_na_r4(delta,alpha,gamma,n)
              ds  = t0-t1
           end if
       end function ray_diff_r4

         
       pure function ray_diff_r8(delta,alpha,gamma,n,u) result(ds)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_diff_r8
           !dir$ attributes forceinline :: ray_diff_r8
           real(kind=dp),   intent(in) :: delta
           real(kind=dp),   intent(in) :: alpha
           real(kind=dp),   intent(in) :: gamma
           real(kind=dp),   intent(in) :: n
           real(kind=dp),   intent(in) :: u
           real(kind=dp) :: ds
           real(kind=dp), automatic :: t0,t1,u2,u2g,su2,sg,t2,n2,t3,t4,t5
           n   = n*n
           u2  = u*0.5_dp
           u2g = u2-gamma
           t2  = sin(u2g)
           su2 = t2*t2
           if(n2>=su2) then
              t3 = (-2.0_dp*delta)/n
              t4 = sin(u2)
              t5 = sin(gamma)
              ds = t3*t4*t5
           else
           t0  = ray_intercept_pa_r8(delta,alpha,gamma,n)
           t1  = ray_intercept_na_r8(delta,alpha,gamma,n)
           ds  = t0-t1
        end function ray_diff_r8


      
         
        !Поле точек пересечения лучей, преломленных пластинкой,
        !относительно оси Ох (рис. 87) имеет симметрию, поэтому 
        !упростим обозначения и выполним расчет соответствующих 
        !координат на основании
        ! Formula 6,7, p. 111
        subroutine compute_dxdy_r4(alpha,beta,delta,gamma,n,u,dx,dy)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_dxdy_r4
           !dir$ attributes forceinline ::  compute_dxdy_r4
           real(kind=sp),    intent(in)  :: alpha
           real(kind=sp),    intent(in)  :: beta
           real(kind=sp),    intent(in)  :: delta
           real(kind=sp),    intent(in)  :: gamma
           real(kind=sp),    intent(in)  :: n
           real(kind=sp),    intent(in)  :: u
           real(kind=sp),    intent(out) :: dx
           real(kind=sp),    intent(out) :: dy
           real(kind=sp), automatic :: ag,ds,t0,t1,t2
           ag  = alpha+gamma
           ds  = ray_diff_r4(delta,alfa,gamma,n,u)
           t0  = sin(ag)
           t1  = 2.0_sp*sin(alpha)
           t2  = 2.0_sp*cos(alpha)
           dx  = t0/t1*ds
           dy  = t0/t2*ds
        end subroutine compute_dxdy_r4


        subroutine compute_dxdy_r8(alpha,beta,delta,gamma,n,u,dx,dy)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_dxdy_r8
           !dir$ attributes forceinline ::  compute_dxdy_r8
           real(kind=dp),    intent(in)  :: alpha
           real(kind=dp),    intent(in)  :: beta
           real(kind=dp),    intent(in)  :: delta
           real(kind=dp),    intent(in)  :: gamma
           real(kind=dp),    intent(in)  :: n
           real(kind=dp),    intent(in)  :: u
           real(kind=dp),    intent(out) :: dx
           real(kind=dp),    intent(out) :: dy
           real(kind=dp), automatic :: ag,ds,t0,t1,t2
           ag  = alpha+gamma
           ds  = ray_diff_r8(delta,alfa,gamma,n,u)
           t0  = sin(ag)
           t1  = 2.0_dp*sin(alpha)
           t2  = 2.0_dp*cos(alpha)
           dx  = t0/t1*ds
           dy  = t0/t2*ds
       end subroutine compute_dxdy_r8


     

        ! Formula 7,8  p. 111
        subroutine compute_xy_r4(alpha,beta,delta,gamma,n,u,x,y)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_xy_r4
           !dir$ attributes forceinline ::  compute_xy_r4
           real(kind=sp),    intent(in)  :: alpha
           real(kind=sp),    intent(in)  :: beta
           real(kind=sp),    intent(in)  :: delta
           real(kind=sp),    intent(in)  :: gamma
           real(kind=sp),    intent(in)  :: n
           real(kind=sp),    intent(in)  :: u
           real(kind=sp),    intent(out) :: x
           real(kind=sp),    intent(out) :: y
           real(kind=sp), automatic :: sag,cag,pa,dx,dy,xs,ys
           sag  = sin(gamma)
           cag  = cos(gamma)
           pa   = ray_intercept_pa_r4(delta,alpha,gamma,n)
           xs   = pa*sag
           ys   = pa*cag
           call compute_dxdy_r4(alpha,beta,delta,gamma,n,u,dx,dy)
           x    = xs+dx
           y    = ys+dx
        end subroutine compute_xy_r4

    
        subroutine compute_xy_r8(alpha,beta,delta,gamma,n,u,x,y)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_xy_r8
           !dir$ attributes forceinline ::  compute_xy_r8
           real(kind=dp),    intent(in)  :: alpha
           real(kind=dp),    intent(in)  :: beta
           real(kind=dp),    intent(in)  :: delta
           real(kind=dp),    intent(in)  :: gamma
           real(kind=dp),    intent(in)  :: n
           real(kind=dp),    intent(in)  :: u
           real(kind=dp),    intent(out) :: x
           real(kind=dp),    intent(out) :: y
           real(kind=dp), automatic :: sag,cag,pa,dx,dy,xs,ys
           sag  = sin(gamma)
           cag  = cos(gamma)
           pa   = ray_intercept_pa_r8(delta,alpha,gamma,n)
           xs   = pa*sag
           ys   = pa*cag
           call compute_dxdy_r8(alpha,beta,delta,gamma,n,u,dx,dy)
           x    = xs+dx
           y    = ys+dx
        end subroutine compute_xy_r8


       


        subroutine compute_xdyd_r4(gamma,u,n,xd,yd)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_xdyd_r4
           !dir$ attributes forceinline ::  compute_xdyd_r4
           real(kind=sp),  intent(in)  :: gamma
           real(kind=sp),  intent(in)  :: u
           real(kind=sp),  intent(in)  :: n
           real(kind=sp),  intent(out) :: xd
           real(kind=sp),  intent(out) :: yd
           real(kind=sp), automatic :: cosg,sing,sin2s,sin2d,u2,u2gs,u2gd,n2,t0,t1,t2,t3,t4
           cosg = cos(gamma)
           sing = sin(gamma)
           u2   = u*0.5_sp
           n2   = n*n
           u2gs = u2+gamma
           ungd = u2-gamma
           t0   = sin(u2gs)
           sin2s= t0*t0
           t1   = sin(u2gd)
           sin2d= t1*t1
           t2   = 1.0_sp/(4.0_sp*sin(u2))
           t3   = sqrt(n2-sin2s)
           t0   = sin2s/t3
           t4   = sqrt(n2-sin2d)
           t1   = sin2d/t4
           dx   = cosg-t2*(t0+t1)
           t2   = 1.0_sp/(4.0_sp*cos(u2))
           dy   = sing-t2*(t0-t1)
        end subroutine compute_xdyd_r4


       subroutine compute_xdyd_r8(gamma,u,n,xd,yd)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  compute_xdyd_r8
           !dir$ attributes forceinline ::  compute_xdyd_r8
           real(kind=dp),  intent(in)  :: gamma
           real(kind=dp),  intent(in)  :: u
           real(kind=dp),  intent(in)  :: n
           real(kind=dp),  intent(out) :: xd
           real(kind=dp),  intent(out) :: yd
           real(kind=dp), automatic :: cosg,sing,sin2s,sin2d,u2,u2gs,u2gd,n2,t0,t1,t2,t3,t4
           cosg = cos(gamma)
           sing = sin(gamma)
           u2   = u*0.5_dp
           n2   = n*n
           u2gs = u2+gamma
           ungd = u2-gamma
           t0   = sin(u2gs)
           sin2s= t0*t0
           t1   = sin(u2gd)
           sin2d= t1*t1
           t2   = 1.0_dp/(4.0_dp*sin(u2))
           t3   = sqrt(n2-sin2s)
           t0   = sin2s/t3
           t4   = sqrt(n2-sin2d)
           t1   = sin2d/t4
           dx   = cosg-t2*(t0+t1)
           t2   = 1.0_dp/(4.0_dp*cos(u2))
           dy   = sing-t2*(t0-t1)
        end subroutine compute_xdyd_r8


       

        subroutine paraxial_xdyd_r4(gamma,alpha,n,xd,yd)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  paraxial_xdyd_r4
           !dir$ attributes forceinline ::  paraxial_xdyd_r4 
           real(kind=sp),  intent(in)  :: gamma
           real(kind=sp),  intent(in)  :: alpha
           real(kind=sp),  intent(in)  :: n
           real(kind=sp),  intent(out) :: xd
           real(kind=sp),  intent(out) :: yd
           real(kind=sp), automatic :: n2,cosg,sing,sin4g,sin2g,num,den,cos2g,t0,t1
           n2    = n*n
           cosg  = cos(gamma)
           cos2g = cos(gamma+gamma)
           sing  = sin(gamma)
           sin4g = sing*sing*sing*sing
           num   = n2*cos2g+sin4g
           den   = (n2-sing*sing)**1.5_sp
           xd    = cosg-num/den
           t0    = sqrt(n2-sing*sing)
           t1    = 1.0_sp-cosg/t0
           yd    = sing*t1
        end subroutine paraxial_xdyd_r4

     
        subroutine paraxial_xdyd_r8(gamma,alpha,n,xd,yd)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  paraxial_xdyd_r8
           !dir$ attributes forceinline ::  paraxial_xdyd_r8
           real(kind=dp),  intent(in)  :: gamma
           real(kind=dp),  intent(in)  :: alpha
           real(kind=dp),  intent(in)  :: n
           real(kind=dp),  intent(out) :: xd
           real(kind=dp),  intent(out) :: yd
           real(kind=dp), automatic :: n2,cosg,sing,sin4g,sin2g,num,den,cos2g,t0,t1
           n2    = n*n
           cosg  = cos(gamma)
           cos2g = cos(gamma+gamma)
           sing  = sin(gamma)
           sin4g = sing*sing*sing*sing
           num   = n2*cos2g+sin4g
           den   = (n2-sing*sing)**1.5_dp
           xd    = cosg-num/den
           t0    = sqrt(n2-sing*sing)
           t1    = 1.0_dp-cosg/t0
           yd    = sing*t1
        end subroutine paraxial_xdyd_r8


       
        
        
       


        !СКАНИРОВАНИЕ ВРАЩАЮЩИМИСЯ ОБЪЕКТИВАМИ
        !Formula 1, p. 121
        subroutine fov_axay_r4(H,delx,dely,phi,ax,ay)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  fov_axay_r4
           !dir$ attributes forceinline ::  fov_axay_r4  
           real(kind=sp),  intent(in) :: H
           real(kind=sp),  intent(in) :: delx
           real(kind=sp),  intent(in) :: dely
           real(kind=sp),  intent(in) :: phi
           real(kind=sp),  intent(out):: ax
           real(kind=sp),  intent(out):: ay
           real(kind=sp), automatic :: sec2,phi2,t0,t1,sec
           phi2  = 0.5_sp*phi
           sec   = 1.0_sp/cos(phi2)
           sec2  = sec*sec
           ax    = H*delx*sec2
           ay    = H*dely*sec
        end subroutine fov_axay_r4
        
        
        subroutine fov_axay_r8(H,delx,dely,phi,ax,ay)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  fov_axay_r8
           !dir$ attributes forceinline ::  fov_axay_r8  
           real(kind=dp),  intent(in) :: H
           real(kind=dp),  intent(in) :: delx
           real(kind=dp),  intent(in) :: dely
           real(kind=dp),  intent(in) :: phi
           real(kind=dp),  intent(out):: ax
           real(kind=dp),  intent(out):: ay
           real(kind=dp), automatic :: sec2,phi2,t0,t1,sec
           phi2  = 0.5_dp*phi
           sec   = 1.0_dp/cos(phi2)
           sec2  = sec*sec
           ax    = H*delx*sec2
           ay    = H*dely*sec
        end subroutine fov_axay_r8
        
        
       
        
        
        subroutine  fov_dxdy_r4(x,y,F,phi,dx,dy)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  fov_dxdy_r4
           !dir$ attributes forceinline ::  fov_dxdy_r4  
           real(kind=sp),   intent(in) :: x
           real(kind=sp),   intent(in) :: y
           real(kind=sp),   intent(in) :: F
           real(kind=sp),   intent(in) :: phi
           real(kind=sp),   intent(out):: dx
           real(kind=sp),   intent(out):: dy
           real(kind=sp), automatic :: d0x,d0y,phi2
           d0y   = y/F
           phi2  = 0.5_sp*phi
           d0x   = x/F
           dy    = d0y
           dx    = d0x*cos(phi2)
        end subroutine fov_dxdy_r4
        
        
        subroutine  fov_dxdy_r8(x,y,F,phi,dx,dy)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  fov_dxdy_r8
           !dir$ attributes forceinline ::  fov_dxdy_r8  
           real(kind=dp),   intent(in) :: x
           real(kind=dp),   intent(in) :: y
           real(kind=dp),   intent(in) :: F
           real(kind=dp),   intent(in) :: phi
           real(kind=dp),   intent(out):: dx
           real(kind=dp),   intent(out):: dy
           real(kind=dp), automatic :: d0x,d0y,phi2
           d0y   = y/F
           phi2  = 0.5_dp*phi
           d0x   = x/F
           dy    = d0y
           dx    = d0x*cos(phi2)
        end subroutine fov_dxdy_r8
        
        
       
        
        subroutine volt_impulse_uxuy_r4(u,om1,om2,t,ux,uy)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  volt_impulse_uxuy_r4
           !dir$ attributes forceinline ::  volt_impulse_uxuy_r4
           real(kind=sp),   intent(in) :: u
           real(kind=sp),   intent(in) :: om1
           real(kind=sp),   intent(in) :: om2
           real(kind=sp),   intent(in) :: t
           real(kind=sp),   intent(out):: ux
           real(kind=sp),   intent(out):: uy
           real(kind=sp), automatic :: om1t,om2t,t0,t1
           om1t = om1*t
           om2t = om2*t
           t0   = sin(om1t)+sin(om2t)
           t1   = cos(om1t)+cos(om2t)
           ux   = u*t0
           uy   = u*t1
        end subroutine volt_impulse_uxuy_r4
        
        
        subroutine volt_impulse_uxuy_r8(u,om1,om2,t,ux,uy)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  volt_impulse_uxuy_r8
           !dir$ attributes forceinline ::  volt_impulse_uxuy_r8
           real(kind=dp),   intent(in) :: u
           real(kind=dp),   intent(in) :: om1
           real(kind=dp),   intent(in) :: om2
           real(kind=dp),   intent(in) :: t
           real(kind=dp),   intent(out):: ux
           real(kind=dp),   intent(out):: uy
           real(kind=dp), automatic :: om1t,om2t,t0,t1
           om1t = om1*t
           om2t = om2*t
           t0   = sin(om1t)+sin(om2t)
           t1   = cos(om1t)+cos(om2t)
           ux   = u*t0
           uy   = u*t1
        end subroutine volt_impulse_uxuy_r8


       


        ! Phase Modulation
        ! Formula 1, p. 143
        ! растрового анализатора со 
        !скрещивающимися осями, выполненного в виде надетой на вращающийся 
        !барабан тонкой пленки, прозрачность которой изменяется по 
        !синусоидальному закону 
        pure elemental function raster_transparency_r4(rho_avg,rho_max,    &
                                                       rho_min,l,L,N) result(rho)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_transparency_r4
           !dir$ attributes forceinline ::  raster_transparency_r4
           real(kind=sp),    intent(in) :: rho_avg
           real(kind=sp),    intent(in) :: rho_max
           real(kind=sp),    intent(in) :: rho_min
           real(kind=sp),    intent(in) :: l
           real(kind=sp),    intent(in) :: L
           real(kind=sp),    intent(in) :: N
           real(kind=sp) :: rho
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp), automatic :: t0,t1,t2
           t0  = 0.5_sp*(rho_max-rho_min)
           t1  = L/N
           t2  = sin(twopi*l*t1)
           rho = rho_avg+t0*t2
        end function raster_transparency_r4


       pure elemental function raster_transparency_r8(rho_avg,rho_max,    &
                                                       rho_min,l,L,N) result(rho)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_transparency_r8
           !dir$ attributes forceinline ::  raster_transparency_r8
           real(kind=dp),    intent(in) :: rho_avg
           real(kind=dp),    intent(in) :: rho_max
           real(kind=dp),    intent(in) :: rho_min
           real(kind=dp),    intent(in) :: l
           real(kind=dp),    intent(in) :: L
           real(kind=dp),    intent(in) :: N
           real(kind=dp) :: rho
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp), automatic :: t0,t1,t2
           t0  = 0.5_dp*(rho_max-rho_min)
           t1  = L/N
           t2  = sin(twopi*l*t1)
           rho = rho_avg+t0*t2
        end function raster_transparency_r8


        !СТРУКТУРА И СПЕКТР МОДУЛИРОВАННОГО ПОТОКА
        !ИЗЛУЧЕНИЯ
        !Formula 1, p. 178
        ! Ф(*) = Int rp(z,t)E(z,t) dsig
        subroutine raster_flux_integral_omp_r8(rhoE,absc,n,t,xlo,xup,Phit_x,ier)
                                             
                                        
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_integral_omp_r8
           use quadpack, only : davint
           real(kind=dp),    dimension(1:n,t), intent(in) :: rhoE
           real(kind=dp),    dimension(1:n),   intent(in) :: absc
           integer(kind=i4),                   intent(in) :: n
           integer(kind=i4),                   intent(in) :: t
           real(kind=dp),                      intent(in) :: xlo
           real(kind=dp),                      intent(in) :: xup
           real(kind=dp),    dimension(t),     intent(out):: Phit
           integer(kind=i4), dimension(t),     intent(out):: ier
          
           real(kind=dp) :: ans
           integer(kind=i4)  :: i
           integer(kind=i4)  :: err
           !dir$ assume_aligned rhoE:64
           !dir$ assume_aligned absc:64
           !dir$ assume_aligned Phit:64
           !dir$ assume_aligned ier:64
!$omp parallel do schedule(runtime) default(none) &
!$omp private(i,ans,err)          &
!$omp shared(t,rhoE,absc,n,xlo,xup)
           do i=1, t ! for 't' time of scanning of radiance field
              call davint(rhoE(:,i),absc,n,xlo,xup,ans,err)
              Phit(i) = ans
              ier(i)  = err
           end do
!$omp end parallel do
        end subroutine raster_flux_integral_omp_r8


        subroutine raster_flux_integral_r8(rhoE,absc,n,t,xlo,xup,Phit,ier)
                                          
                                        
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_integral_r8
           use quadpack, only : davint
           real(kind=dp),    dimension(1:n,t), intent(in) :: rhoE
           real(kind=dp),    dimension(1:n),   intent(in) :: absc
           integer(kind=i4),                   intent(in) :: n
           integer(kind=i4),                   intent(in) :: t
           real(kind=dp),                      intent(in) :: xlo
           real(kind=dp),                      intent(in) :: xup
           real(kind=dp),    dimension(t),     intent(out):: Phit
           integer(kind=i4), dimension(t),     intent(out):: ier
          
           real(kind=dp) :: ans
           integer(kind=i4)  :: i
           integer(kind=i4)  :: err
           !dir$ assume_aligned rhoE:64
           !dir$ assume_aligned absc:64
           !dir$ assume_aligned Phit:64
           !dir$ assume_aligned ier:64

           do i=1, t ! for 't' time of scanning of radiance field
              call davint(rhoE(:,i),absc,n,xlo,xup,ans,err)
              Phit(i) = ans
              ier(i)  = err
           end do

         end subroutine raster_flux_integral_r8


         subroutine raster_flux_integral_omp_r4(rhoE,absc,n,t,xlo,xup,Phit,ier)
                                              
                                        
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_integral_omp_r4
           use quadpack, only : savint
           real(kind=sp),    dimension(1:n,t), intent(in) :: rhoE
           real(kind=sp),    dimension(1:n),   intent(in) :: absc
           integer(kind=i4),                   intent(in) :: n
           integer(kind=i4),                   intent(in) :: t
           real(kind=sp),                      intent(in) :: xlo
           real(kind=sp),                      intent(in) :: xup
           real(kind=sp),    dimension(t),     intent(out):: Phit
           integer(kind=i4), dimension(t),     intent(out):: ier
       
           real(kind=sp) :: ans
           integer(kind=i4)  :: i
           integer(kind=i4)  :: err 
           !dir$ assume_aligned rhoE:64
           !dir$ assume_aligned Phit:64
           !dir$ assume_aligned absc:64
           !dir$ assume_aligned ier:64
!$omp parallel do schedule(runtime) default(none) &
!$omp private(i,ans,err)          &
!$omp shared(t,rhoE,absc,n,xlo,xup,Phit,ier)
           do i=1, t ! for 't' time of scanning of radiance field
              call savint(rhoE(:,i),absc,n,xlo,xup,ans,err)
              Phit(i) = ans
              ier(i)  = err
            end do
!$omp end parallel do
        end subroutine raster_flux_integral_omp_r4


        subroutine raster_flux_integral_r4(rhoE,absc,n,t,xlo,xup,Phit,ier)
                                           
                                        
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_integral_r4
           use quadpack, only : savint
           real(kind=sp),    dimension(1:n,t), intent(in) :: rhoE
           real(kind=sp),    dimension(1:n),   intent(in) :: absc
           integer(kind=i4),                   intent(in) :: n
           integer(kind=i4),                   intent(in) :: t
           real(kind=sp),                      intent(in) :: xlo
           real(kind=sp),                      intent(in) :: xup
           real(kind=sp),    dimension(t),     intent(out):: Phit
           integer(kind=i4), dimension(t),     intent(out):: ier
          
           real(kind=sp) :: ans
           integer(kind=i4)  :: i
           integer(kind=i4)  :: err
           !dir$ assume_aligned rhoE:64
           !dir$ assume_aligned Phit:64
           !dir$ assume_aligned absc:64
           !dir$ assume_aligned ier:64
           do i=1, t ! for 't' time of scanning of radiance field
              call savint(rhoE(:,i),absc,n,xlo,xup,ans,err)
              Phit(i) = ans
              ier(i)  = err
             
           end do

        end subroutine raster_flux_integral_r4



        ! Formula 3, p. 180
        subroutine raster_opacity_integral_omp_r8(invs,rhophi,absc,n,t,xlo,xup,rho,ier)
                                              

            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  raster_opacity_integral_omp_r8
            use quadpack, only : davint
            real(kind=dp),                      intent(in) :: invs
            real(kind=dp), dimension(1:n,t),    intent(in) :: rhophi
            real(kind=dp), dimension(1:n),      intent(in) :: absc
            integer(kind=i4),                   intent(in) :: n
            integer(kind=i4),                   intent(in) :: t
            real(kind=dp),                      intent(in) :: xlo
            real(kind=dp),                      intent(in) :: xup
            real(kind=dp),    dimension(t),     intent(out):: rho
            integer(kind=i4), dimension(t),     intent(out):: ier
            
            real(kind=dp) :: ans
            integer(kind=i4)  :: i
            integer(kind=i4)  :: err
            !dir$ assume_aligned rhophi:64
            !dir$ assume_aligned absc:64
            !dir$ assume_aligned ier:64
            !dir$ assume_aligned rho:64


!$omp parallel do schedule(runtime)default(none) &
!$omp private(i,ans,err)         &
!$omp shared(t,rhophi,absc,n,xlo,xup) &
!$omp shared(rho,ier)
            do i=1, t
               call davint(rhophi(:,t),absc,n,xlo,xup,ans,err)
               rho(i) = invs*ans
               ier(i) = err
             
            end do
!$omp end parallel do
       end subroutine raster_opacity_integral_omp_r8


       subroutine raster_opacity_integral_r8(invs,rhophi,absc,n,t,xlo,xup,rho,ier) 
                                              

            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  raster_opacity_integral_r8
            use quadpack, only : davint
            real(kind=dp),                      intent(in) :: invs
            real(kind=dp), dimension(1:n,t),    intent(in) :: rhophi
            real(kind=dp), dimension(1:n),      intent(in) :: absc
            integer(kind=i4),                   intent(in) :: n
            integer(kind=i4),                   intent(in) :: t
            real(kind=dp),                      intent(in) :: xlo
            real(kind=dp),                      intent(in) :: xup
            real(kind=dp),    dimension(t),     intent(out):: rho
            integer(kind=i4), dimension(t),     intent(out):: ier
         
            real(kind=dp) :: ans
            integer(kind=i4)  :: i
            integer(kind=i4)  :: err
            !dir$ assume_aligned rhophi:64
            !dir$ assume_aligned absc:64
            !dir$ assume_aligned rho:64
            !dir$ assume_aligned ier:64

            do i=1, t
               call davint(rhophi(:,t),absc,n,xlo,xup,ans,err)
               rho(i) = invs*ans
               ier(i) = err
              
            end do

       end subroutine raster_opacity_integral_r8


       subroutine raster_opacity_integral_omp_r4(invs,rhophi_x,rhophi_y,absc,n,t,xlo, &
                                               xup,rho_x,rho_y,ier_x,ier_y)

            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  raster_opacity_integral_omp_r4
            use quadpack, only : savint
            real(kind=sp),                      intent(in) :: invs
            real(kind=sp), dimension(1:n,t),    intent(in) :: rhophi
            real(kind=sp), dimension(1:n),      intent(in) :: absc
            integer(kind=i4),                   intent(in) :: n
            integer(kind=i4),                   intent(in) :: t
            real(kind=sp),                      intent(in) :: xlo
            real(kind=sp),                      intent(in) :: xup
            real(kind=sp),    dimension(t),     intent(out):: rho
  
            integer(kind=i4), dimension(t),     intent(out):: ier
         
            real(kind=sp) :: ans
            integer(kind=i4)  :: i
            integer(kind=i4)  :: err
            !dir$ assume_aligned rhophi:64
            !dir$ assume_aligned absc:64
            !dir$ assume_aligned rho:64


!$omp parallel do schedule(runtime)default(none) &
!$omp private(i,ans,err) shared(t,rhophi,absc,n,xlo,xup)        &
            do i=1, t
               call savint(rhophi(:,t),absc,n,xlo,xup,ans,err)
               rho(i) = invs*ans
               ier(i) = err
            end do
!$omp end parallel do
       end subroutine raster_opacity_integral_omp_r4


       subroutine raster_opacity_integral_r4(invs,rhophi,absc,n,t,xlo, &
                                               xup,rho,ier_x)

            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  raster_opacity_integral_r4
            use quadpack, only : savint
            real(kind=sp),                      intent(in) :: invs
            real(kind=sp), dimension(1:n,t),    intent(in) :: rhophi
            real(kind=sp), dimension(1:n),      intent(in) :: absc
            integer(kind=i4),                   intent(in) :: n
            integer(kind=i4),                   intent(in) :: t
            real(kind=sp),                      intent(in) :: xlo
            real(kind=sp),                      intent(in) :: xup
            real(kind=sp),    dimension(t),     intent(out):: rho
            integer(kind=i4), dimension(t),     intent(out):: ier
         
            real(kind=sp) :: ans
            integer(kind=i4)  :: i
            integer(kind=i4)  :: err
            !dir$ assume_aligned rhophi:64
            !dir$ assume_aligned absc:64
            !dir$ assume_aligned rho:64
            !dir$ assume_aligned ier:64

            do i=1, t
               call savint(rhophi(:,t),absc,n,xlo,xup,ans,err)
               rho(i) = invs*ans
               ier(i) = err
              
            end do

       end subroutine raster_opacity_integral_r4

              
 
       subroutine cos_series_unroll_16x_r4(om0,n,coss,k)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  cos_series_unroll_16x_r4
           !dir$ attributes forceinline ::  cos_series_unroll_16x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: cos_series_unroll_16x_r4
           real(kind=sp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp), dimension(1:n), intent(out) :: coss
           real(kind=sp),                 intent(in)  :: k
           real(kind=sp) :: arg0,arg1,arg2,arg3,arg4,arg,arg6,arg7
           real(kind=sp) :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           real(kind=sp) :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=sp) :: t8,t9,t10,t11,t12,t13,t14,t15
           real(kind=sp) :: kom0
           integer(kind=i4) :: i,m,m1
           kom0 = k*om0
           m = mod(n,16)
           if(m /= 0) then
              do i=1, m
                 t0      = real(i,kind=sp)
                 arg0    = kom0*t0
                 coss(i) = cos(arg0)
              end do
              if(n<16) return
           end if
           m1 = m+1
            !dir$ assume_aligned coss:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
           do i=m1,n,16
              t0        = real(i+0,kind=sp)
              arg0      = kom0*t0
              coss(i+0) = cos(arg0)
              t1        = real(i+1,kind=sp)
              arg1      = kom0*t1
              coss(i+1) = cos(arg1)
              t2        = real(i+2,kind=sp)
              arg2      = kom0*t2
              coss(i+2) = cos(arg2)
              t3        = real(i+3,kind=sp)
              arg3      = kom0*t3
              coss(i+3) = cos(arg3)
              t4        = real(i+4,kind=sp)
              arg4      = kom0*t4
              coss(i+4) = cos(arg4)
              t5        = real(i+5,kind=sp)
              arg5      = kom0*t5
              coss(i+5) = cos(arg5)
              t6        = real(i+6,kind=sp)
              arg6      = kom0*t6
              coss(i+6) = cos(arg6)
              t7        = real(i+7,kind=sp)
              arg7      = kom0*t7
              coss(i+7) = cos(arg7)
              t8        = real(i+8,kind=sp)
              arg8      = kom0*t8
              coss(i+8) = cos(arg8)
              t9        = real(i+9,kind=sp)
              arg9      = kom0*t9
              coss(i+9) = cos(arg9)
              t10       = real(i+10,kind=sp)
              arg10     = kom0*t10
              coss(i+10)= cos(arg10)
              t11       = real(i+11,kind=sp)
              arg11     = kom0*t11
              coss(i+11)= cos(arg11)
              t12       = real(i+12,kind=sp)
              arg12     = kom0*t12
              coss(i+12)= cos(arg12)
              t13       = real(i+13,kind=sp)
              arg13     = kom0*t13
              coss(i+13)= cos(arg13)
              t14       = real(i+14,kind=sp)
              arg14     = kom0*t14
              coss(i+14)= cos(arg14)
              t15       = real(i+15,kind=sp)
              arg15     = kom0*t15
              coss(i+15)= cos(arg15)
           end do
           
       end subroutine cos_series_unroll_16x_r4


       subroutine cos_series_unroll_16x_r8(om0,n,coss,k)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  cos_series_unroll_16x_r8
           !dir$ attributes forceinline ::  cos_series_unroll_16x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: cos_series_unroll_16x_r8
           real(kind=dp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp), dimension(1:n), intent(out) :: coss
           real(kind=dp),                 intent(in)  :: k
           real(kind=dp) :: arg0,arg1,arg2,arg3,arg4,arg,arg6,arg7
           real(kind=dp) :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           real(kind=dp) :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=dp) :: t8,t9,t10,t11,t12,t13,t14,t15
           real(kind=dp) :: kom0
           integer(kind=i4) :: i,m,m1
           kom0 = k*om0
           m = mod(n,16)
           if(m /= 0) then
              do i=1, m
                 t0      = real(i,kind=sp)
                 arg0    = kom0*t0
                 coss(i) = cos(arg0)
              end do
              if(n<16) return
           end if
           m1 = m+1
            !dir$ assume_aligned coss:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
           do i=m1,n,16
              t0        = real(i+0,kind=dp)
              arg0      = kom0*t0
              coss(i+0) = cos(arg0)
              t1        = real(i+1,kind=dp)
              arg1      = kom0*t1
              coss(i+1) = cos(arg1)
              t2        = real(i+2,kind=dp)
              arg2      = kom0*t2
              coss(i+2) = cos(arg2)
              t3        = real(i+3,kind=dp)
              arg3      = kom0*t3
              coss(i+3) = cos(arg3)
              t4        = real(i+4,kind=dp)
              arg4      = kom0*t4
              coss(i+4) = cos(arg4)
              t5        = real(i+5,kind=dp)
              arg5      = kom0*t5
              coss(i+5) = cos(arg5)
              t6        = real(i+6,kind=dp)
              arg6      = kom0*t6
              coss(i+6) = cos(arg6)
              t7        = real(i+7,kind=dp)
              arg7      = kom0*t7
              coss(i+7) = cos(arg7)
              t8        = real(i+8,kind=dp)
              arg8      = kom0*t8
              coss(i+8) = cos(arg8)
              t9        = real(i+9,kind=dp)
              arg9      = kom0*t9
              coss(i+9) = cos(arg9)
              t10       = real(i+10,kind=dp)
              arg10     = kom0*t10
              coss(i+10)= cos(arg10)
              t11       = real(i+11,kind=dp)
              arg11     = kom0*t11
              coss(i+11)= cos(arg11)
              t12       = real(i+12,kind=dp)
              arg12     = kom0*t12
              coss(i+12)= cos(arg12)
              t13       = real(i+13,kind=dp)
              arg13     = kom0*t13
              coss(i+13)= cos(arg13)
              t14       = real(i+14,kind=dp)
              arg14     = kom0*t14
              coss(i+14)= cos(arg14)
              t15       = real(i+15,kind=dp)
              arg15     = kom0*t15
              coss(i+15)= cos(arg15)
           end do
           
       end subroutine cos_series_unroll_16x_r8


      subroutine cos_series_unroll_8x_r4(om0,n,coss,k)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  cos_series_unroll_8x_r4
           !dir$ attributes forceinline ::  cos_series_unroll_8x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: cos_series_unroll_8x_r4
           real(kind=sp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp), dimension(1:n), intent(out) :: coss
           real(kind=sp),                 intent(in)  :: k
           real(kind=sp) :: arg0,arg1,arg2,arg3,arg4,arg,arg6,arg7
           real(kind=sp) :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=sp) :: kom0
           integer(kind=i4) :: i,m,m1
           kom0 = k*om0
           m = mod(n,8)
           if(m /= 0) then
              do i=1, m
                 t0      = real(i,kind=sp)
                 arg0    = kom0*t0
                 coss(i) = cos(arg0)
              end do
              if(n<8) return
           end if
           m1 = m+1
            !dir$ assume_aligned coss:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
           do i=m1,n,8
              t0        = real(i+0,kind=sp)
              arg0      = kom0*t0
              coss(i+0) = cos(arg0)
              t1        = real(i+1,kind=sp)
              arg1      = kom0*t1
              coss(i+1) = cos(arg1)
              t2        = real(i+2,kind=sp)
              arg2      = kom0*t2
              coss(i+2) = cos(arg2)
              t3        = real(i+3,kind=sp)
              arg3      = kom0*t3
              coss(i+3) = cos(arg3)
              t4        = real(i+4,kind=sp)
              arg4      = kom0*t4
              coss(i+4) = cos(arg4)
              t5        = real(i+5,kind=sp)
              arg5      = kom0*t5
              coss(i+5) = cos(arg5)
              t6        = real(i+6,kind=sp)
              arg6      = kom0*t6
              coss(i+6) = cos(arg6)
              t7        = real(i+7,kind=sp)
              arg7      = kom0*t7
              coss(i+7) = cos(arg7)
          end do
           
       end subroutine cos_series_unroll_8x_r4


       subroutine cos_series_unroll_8x_r8(om0,n,coss,k)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  cos_series_unroll_8x_r8
           !dir$ attributes forceinline ::  cos_series_unroll_8x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: cos_series_unroll_8x_r8
           real(kind=dp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp), dimension(1:n), intent(out) :: coss
           real(kind=dp),                 intent(in)  :: k
           real(kind=dp) :: arg0,arg1,arg2,arg3,arg4,arg,arg6,arg7
           real(kind=dp) :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=dp) :: kom0
           integer(kind=i4) :: i,m,m1
           kom0 = k*om0
           m = mod(n,8)
           if(m /= 0) then
              do i=1, m
                 t0      = real(i,kind=sp)
                 arg0    = kom0*t0
                 coss(i) = cos(arg0)
              end do
              if(n<8) return
           end if
           m1 = m+1
            !dir$ assume_aligned coss:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
           do i=m1,n,8
              t0        = real(i+0,kind=dp)
              arg0      = kom0*t0
              coss(i+0) = cos(arg0)
              t1        = real(i+1,kind=dp)
              arg1      = kom0*t1
              coss(i+1) = cos(arg1)
              t2        = real(i+2,kind=dp)
              arg2      = kom0*t2
              coss(i+2) = cos(arg2)
              t3        = real(i+3,kind=dp)
              arg3      = kom0*t3
              coss(i+3) = cos(arg3)
              t4        = real(i+4,kind=dp)
              arg4      = kom0*t4
              coss(i+4) = cos(arg4)
              t5        = real(i+5,kind=dp)
              arg5      = kom0*t5
              coss(i+5) = cos(arg5)
              t6        = real(i+6,kind=dp)
              arg6      = kom0*t6
              coss(i+6) = cos(arg6)
              t7        = real(i+7,kind=dp)
              arg7      = kom0*t7
              coss(i+7) = cos(arg7)
          end do
     
       end subroutine cos_series_unroll_8x_r8


       subroutine cos_series_unroll_4x_r4(om0,n,coss,k)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  cos_series_unroll_4x_r4
           !dir$ attributes forceinline ::  cos_series_unroll_4x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: cos_series_unroll_4x_r4
           real(kind=sp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp), dimension(1:n), intent(out) :: coss
           real(kind=sp),                 intent(in)  :: k
           real(kind=sp) :: arg0,arg1,arg2,arg3
           real(kind=sp) :: t0,t1,t2,t3
           real(kind=sp) :: kom0
           integer(kind=i4) :: i,m,m1
           kom0 = k*om0
           m = mod(n,4)
           if(m /= 0) then
              do i=1, m
                 t0      = real(i,kind=sp)
                 arg0    = kom0*t0
                 coss(i) = cos(arg0)
              end do
              if(n<4) return
           end if
           m1 = m+1
            !dir$ assume_aligned coss:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
           do i=m1,n,4
              t0        = real(i+0,kind=sp)
              arg0      = kom0*t0
              coss(i+0) = cos(arg0)
              t1        = real(i+1,kind=sp)
              arg1      = kom0*t1
              coss(i+1) = cos(arg1)
              t2        = real(i+2,kind=sp)
              arg2      = kom0*t2
              coss(i+2) = cos(arg2)
              t3        = real(i+3,kind=sp)
              arg3      = kom0*t3
              coss(i+3) = cos(arg3)
           end do
           
       end subroutine cos_series_unroll_4x_r4


       subroutine cos_series_unroll_4x_r8(om0,n,coss,k)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  cos_series_unroll_4x_r8
           !dir$ attributes forceinline ::  cos_series_unroll_4x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: cos_series_unroll_4x_r8
           real(kind=dp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp), dimension(1:n), intent(out) :: coss
           real(kind=dp),                 intent(in)  :: k
           real(kind=dp) :: arg0,arg1,arg2,arg3
           real(kind=dp) :: t0,t1,t2,t3
           real(kind=dp) :: kom0
           integer(kind=i4) :: i,m,m1
           kom0 = k*om0
           m = mod(n,4)
           if(m /= 0) then
              do i=1, m
                 t0      = real(i,kind=sp)
                 arg0    = kom0*t0
                 coss(i) = cos(arg0)
              end do
              if(n<4) return
           end if
           m1 = m+1
            !dir$ assume_aligned coss:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
           do i=m1,n,4
              t0        = real(i+0,kind=dp)
              arg0      = kom0*t0
              coss(i+0) = cos(arg0)
              t1        = real(i+1,kind=dp)
              arg1      = kom0*t1
              coss(i+1) = cos(arg1)
              t2        = real(i+2,kind=dp)
              arg2      = kom0*t2
              coss(i+2) = cos(arg2)
              t3        = real(i+3,kind=dp)
              arg3      = kom0*t3
              coss(i+3) = cos(arg3)
            
          end do
     
       end subroutine cos_series_unroll_4x_r8


        subroutine cos_series_unroll_2x_r4(om0,n,coss,k)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  cos_series_unroll_2x_r4
           !dir$ attributes forceinline ::  cos_series_unroll_2x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: cos_series_unroll_2x_r4
           real(kind=sp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp), dimension(1:n), intent(out) :: coss
           real(kind=sp),                 intent(in)  :: k
           real(kind=sp) :: arg0,arg1
           real(kind=sp) :: t0,t1
           real(kind=sp) :: kom0
           integer(kind=i4) :: i,m,m1
           kom0 = k*om0
           m = mod(n,2)
           if(m /= 0) then
              do i=1, m
                 t0      = real(i,kind=sp)
                 arg0    = kom0*t0
                 coss(i) = cos(arg0)
              end do
              if(n<2) return
           end if
           m1 = m+1
            !dir$ assume_aligned coss:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
           do i=m1,n,2
              t0        = real(i+0,kind=sp)
              arg0      = kom0*t0
              coss(i+0) = cos(arg0)
              t1        = real(i+1,kind=sp)
              arg1      = kom0*t1
              coss(i+1) = cos(arg1)
           end do
           
       end subroutine cos_series_unroll_2x_r4


       subroutine cos_series_r4(om0,n,coss,k)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  cos_series_r4
           !dir$ attributes forceinline ::  cos_series_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: cos_series_r4
           real(kind=sp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp), dimension(1:n), intent(out) :: coss
           real(kind=sp),                 intent(in)  :: k
           real(kind=sp) :: arg0
           real(kind=sp) :: t0
           real(kind=sp) :: kom0
           integer(kind=i4) :: i
           kom0 = k*om0
          
            !dir$ assume_aligned coss:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
           do i=1,n
              t0        = real(i+0,kind=sp)
              arg0      = kom0*t0
              coss(i+0) = cos(arg0)
           end do
           
       end subroutine cos_series_r4


       


       subroutine cos_series_unroll_2x_r8(om0,n,coss,k)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  cos_series_unroll_2x_r8
           !dir$ attributes forceinline ::  cos_series_unroll_2x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: cos_series_unroll_2x_r8
           real(kind=dp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp), dimension(1:n), intent(out) :: coss
           real(kind=dp),                 intent(in)  :: k
           real(kind=dp) :: arg0,arg1
           real(kind=dp) :: t0,t1
           real(kind=dp) :: kom0
           integer(kind=i4) :: i,m,m1
           kom0 = k*om0
           m = mod(n,2)
           if(m /= 0) then
              do i=1, m
                 t0      = real(i,kind=sp)
                 arg0    = kom0*t0
                 coss(i) = cos(arg0)
              end do
              if(n<2) return
           end if
           m1 = m+1
            !dir$ assume_aligned coss:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
           do i=m1,n,2
              t0        = real(i+0,kind=dp)
              arg0      = kom0*t0
              coss(i+0) = cos(arg0)
              t1        = real(i+1,kind=dp)
              arg1      = kom0*t1
              coss(i+1) = cos(arg1)
                       
          end do
     
       end subroutine cos_series_unroll_2x_r8


       subroutine cos_series_r8(om0,n,coss,k)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  cos_series_r8
           !dir$ attributes forceinline ::  cos_series_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: cos_series_r8
           real(kind=dp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp), dimension(1:n), intent(out) :: coss
           real(kind=dp),                 intent(in)  :: k
           real(kind=dp) :: arg0
           real(kind=dp) :: t0
           real(kind=dp) :: kom0
           integer(kind=i4) :: i
           kom0 = k*om0
          
            !dir$ assume_aligned coss:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
           do i=1,n
              t0        = real(i+0,kind=dp)
              arg0      = kom0*t0
              coss(i+0) = cos(arg0)
           end do
           
       end subroutine cos_series_r8


       subroutine cos_series_exec_r4(om0,n,coss,k,unroll_cnt)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  cos_series_exec_r4
           real(kind=sp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp), dimension(1:n), intent(out) :: coss
           real(kind=sp),                 intent(in)  :: k
           integer(kind=i4),              intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                call cos_series_unroll_16x_r4(om0,n,coss,k)
              case (8)
                call cos_series_unroll_8x_r4(om0,n,coss,k)
              case (4)
                call cos_series_unroll_4x_r4(om0,n,coss,k)
              case (2)
                call cos_series_unroll_2x_r4(om0,n,coss,k)
              case (0)
                call cos_series_r4(om0,n,coss,k)
              case default
                return
            end select
       end subroutine cos_series_exec_r4


       subroutine cos_series_exec_r8(om0,n,coss,k,unroll_cnt)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  cos_series_exec_r8
           real(kind=dp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp), dimension(1:n), intent(out) :: coss
           real(kind=dp),                 intent(in)  :: k
           integer(kind=i4),              intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                call cos_series_unroll_16x_r8(om0,n,coss,k)
              case (8)
                call cos_series_unroll_8x_r8(om0,n,coss,k)
              case (4)
                call cos_series_unroll_4x_r8(om0,n,coss,k)
              case (2)
                call cos_series_unroll_2x_r8(om0,n,coss,k)
              case (0)
                call cos_series_r8(om0,n,coss,k)
              case default
                return
            end select
       end subroutine cos_series_exec_r8
      
      

       !==========================================================================================
       
         subroutine sin_series_unroll_16x_r4(om0,n,sins,k)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  sin_series_unroll_16x_r4
           !dir$ attributes forceinline ::  sin_series_unroll_16x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: sin_series_unroll_16x_r4
           real(kind=sp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp), dimension(1:n), intent(out) :: sins
           real(kind=sp),                 intent(in)  :: k
           real(kind=sp) :: arg0,arg1,arg2,arg3,arg4,arg,arg6,arg7
           real(kind=sp) :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           real(kind=sp) :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=sp) :: t8,t9,t10,t11,t12,t13,t14,t15
           real(kind=sp) :: kom0
           integer(kind=i4) :: i,m,m1
           kom0 = k*om0
           m = mod(n,16)
           if(m /= 0) then
              do i=1, m
                 t0        = real(i+0,kind=sp)
                 arg0      = kom0*t0
                 sins(i+0) = sin(arg0)
              end do
              if(n<16) return
           end if
           m1 = m+1
            !dir$ assume_aligned sins:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
           do i=m1,n,16
              t0        = real(i+0,kind=sp)
              arg0      = kom0*t0
              sins(i+0) = sin(arg0)
              t1        = real(i+1,kind=sp)
              arg1      = kom0*t1
              sins(i+1) = sin(arg1)
              t2        = real(i+2,kind=sp)
              arg2      = kom0*t2
              sins(i+2) = sin(arg2)
              t3        = real(i+3,kind=sp)
              arg3      = kom0*t3
              sins(i+3) = sin(arg3)
              t4        = real(i+4,kind=sp)
              arg4      = kom0*t4
              sins(i+4) = sin(arg4)
              t5        = real(i+5,kind=sp)
              arg5      = kom0*t5
              sins(i+5) = sin(arg5)
              t6        = real(i+6,kind=sp)
              arg6      = kom0*t6
              sins(i+6) = sin(arg6)
              t7        = real(i+7,kind=sp)
              arg7      = kom0*t7
              sins(i+7) = sin(arg7)
              t8        = real(i+8,kind=sp)
              arg8      = kom0*t8
              sins(i+8) = sin(arg8)
              t9        = real(i+9,kind=sp)
              arg9      = kom0*t9
              sins(i+9) = sin(arg9)
              t10       = real(i+10,kind=sp)
              arg10     = kom0*t10
              sins(i+10)= sin(arg10)
              t11       = real(i+11,kind=sp)
              arg11     = kom0*t11
              sins(i+11)= sin(arg11)
              t12       = real(i+12,kind=sp)
              arg12     = kom0*t12
              sins(i+12)= sin(arg12)
              t13       = real(i+13,kind=sp)
              arg13     = kom0*t13
              sins(i+13)= sin(arg13)
              t14       = real(i+14,kind=sp)
              arg14     = kom0*t14
              sins(i+14)= sin(arg14)
              t15       = real(i+15,kind=sp)
              arg15     = kom0*t15
              sins(i+15)= sin(arg15)
           end do
          
       end subroutine sin_series_unroll_16x_r4


       subroutine sin_series_unroll_16x_r8(om0,n,sins,k)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  sin_series_unroll_16x_r8
           !dir$ attributes forceinline ::  sin_series_unroll_16x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: sin_series_unroll_16x_r8
           real(kind=dp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp), dimension(1:n), intent(out) :: sins
           real(kind=dp),                 intent(in)  :: k
           real(kind=dp) :: arg0,arg1,arg2,arg3,arg4,arg,arg6,arg7
           real(kind=dp) :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           real(kind=dp) :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=dp) :: t8,t9,t10,t11,t12,t13,t14,t15
           real(kind=dp) :: kom0
           integer(kind=i4) :: i,m,m1
           kom0 = k*om0
           m = mod(n,16)
           if(m /= 0) then
              do i=1, m
                 t0        = real(i,kind=dp)
                 arg0      = kom0*t0
                 sins(i)   = sin(arg0)
              end do
              if(n<16) return
           end if
           m1 = m+1
            !dir$ assume_aligned sins:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
           do i=m1,n,16
              t0        = real(i+0,kind=dp)
              arg0      = kom0*t0
              sins(i+0) = sin(arg0)
              t1        = real(i+1,kind=dp)
              arg1      = kom0*t1
              sins(i+1) = sin(arg1)
              t2        = real(i+2,kind=dp)
              arg2      = kom0*t2
              sins(i+2) = sin(arg2)
              t3        = real(i+3,kind=dp)
              arg3      = kom0*t3
              sins(i+3) = sin(arg3)
              t4        = real(i+4,kind=dp)
              arg4      = kom0*t4
              sins(i+4) = sin(arg4)
              t5        = real(i+5,kind=dp)
              arg5      = kom0*t5
              sins(i+5) = sin(arg5)
              t6        = real(i+6,kind=dp)
              arg6      = kom0*t6
              sins(i+6) = sin(arg6)
              t7        = real(i+7,kind=dp)
              arg7      = kom0*t7
              sins(i+7) = sin(arg7)
              t8        = real(i+8,kind=dp)
              arg8      = kom0*t8
              sins(i+8) = sin(arg8)
              t9        = real(i+9,kind=dp)
              arg9      = kom0*t9
              sins(i+9) = sin(arg9)
              t10       = real(i+10,kind=dp)
              arg10     = kom0*t10
              sins(i+10)= sin(arg10)
              t11       = real(i+11,kind=dp)
              arg11     = kom0*t11
              sins(i+11)= sin(arg11)
              t12       = real(i+12,kind=dp)
              arg12     = kom0*t12
              sins(i+12)= sin(arg12)
              t13       = real(i+13,kind=dp)
              arg13     = kom0*t13
              sins(i+13)= sin(arg13)
              t14       = real(i+14,kind=dp)
              arg14     = kom0*t14
              sins(i+14)= sin(arg14)
              t15       = real(i+15,kind=dp)
              arg15     = kom0*t15
              sins(i+15)= sin(arg15)
           end do
          
       end subroutine sin_series_unroll_16x_r8


      




       subroutine sin_series_unroll_8x_r4(om0,n,sins,k)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  sin_series_unroll_8x_r4
           !dir$ attributes forceinline ::  sin_series_unroll_8x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: sin_series_unroll_8x_r4
           real(kind=sp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp), dimension(1:n), intent(out) :: sins
           real(kind=sp),                 intent(in)  :: k
           real(kind=sp) :: arg0,arg1,arg2,arg3,arg4,arg,arg6,arg7
           real(kind=sp) :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=sp) :: kom0
           integer(kind=i4) :: i,m,m1
           kom0 = k*om0
           m = mod(n,8)
           if(m /= 0) then
              do i=1, m
                 t0        = real(i,kind=sp)
                 arg0      = kom0*t0
                 sins(i)   = sin(arg0)
              end do
              if(n<8) return
           end if
           m1 = m+1
            !dir$ assume_aligned coss:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
           do i=m1,n,8
              t0        = real(i+0,kind=sp)
              arg0      = kom0*t0
              sins(i+0) = sin(arg0)
              t1        = real(i+1,kind=sp)
              arg1      = kom0*t1
              sins(i+1) = sin(arg1)
              t2        = real(i+2,kind=sp)
              arg2      = kom0*t2
              sins(i+2) = sin(arg2)
              t3        = real(i+3,kind=sp)
              arg3      = kom0*t3
              sins(i+3) = sin(arg3)
              t4        = real(i+4,kind=sp)
              arg4      = kom0*t4
              sins(i+4) = sin(arg4)
              t5        = real(i+5,kind=sp)
              arg5      = kom0*t5
              sins(i+5) = sin(arg5)
              t6        = real(i+6,kind=sp)
              arg6      = kom0*t6
              sins(i+6) = sin(arg6)
              t7        = real(i+7,kind=sp)
              arg7      = kom0*t7
              sins(i+7) = sin(arg7)
           end do
           
       end subroutine sin_series_unroll_8x_r4
  

       subroutine sin_series_unroll_8x_r8(om0,n,sins,k)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  sin_series_unroll_8x_r8
           !dir$ attributes forceinline ::  sin_series_unroll_8x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: sin_series_unroll_8x_r8
           real(kind=dp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp), dimension(1:n), intent(out) :: sins
           real(kind=dp),                 intent(in)  :: k
           real(kind=dp) :: arg0,arg1,arg2,arg3,arg4,arg,arg6,arg7
           real(kind=dp) :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=dp) :: kom0
           integer(kind=i4) :: i,m,m1
           kom0 = k*om0
           m = mod(n,8)
           if(m /= 0) then
              do i=1, m
                 t0        = real(i,kind=dp)
                 arg0      = kom0*t0
                 sins(i)   = sin(arg0)
              end do
              if(n<8) return
           end if
           m1 = m+1
            !dir$ assume_aligned coss:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
           do i=m1,n,8
              t0        = real(i+0,kind=dp)
              arg0      = kom0*t0
              sins(i+0) = sin(arg0)
              t1        = real(i+1,kind=dp)
              arg1      = kom0*t1
              sins(i+1) = sin(arg1)
              t2        = real(i+2,kind=dp)
              arg2      = kom0*t2
              sins(i+2) = sin(arg2)
              t3        = real(i+3,kind=dp)
              arg3      = kom0*t3
              sins(i+3) = sin(arg3)
              t4        = real(i+4,kind=dp)
              arg4      = kom0*t4
              sins(i+4) = sin(arg4)
              t5        = real(i+5,kind=dp)
              arg5      = kom0*t5
              sins(i+5) = sin(arg5)
              t6        = real(i+6,kind=dp)
              arg6      = kom0*t6
              sins(i+6) = sin(arg6)
              t7        = real(i+7,kind=dp)
              arg7      = kom0*t7
              sins(i+7) = sin(arg7)
           end do
           
       end subroutine sin_series_unroll_8x_r8
  

       subroutine sin_series_unroll_4x_r4(om0,n,sins,k)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  sin_series_unroll_8x_r4
           !dir$ attributes forceinline ::  sin_series_unroll_8x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: sin_series_unroll_8x_r4
           real(kind=sp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp), dimension(1:n), intent(out) :: sins
           real(kind=sp),                 intent(in)  :: k
           real(kind=sp) :: arg0,arg1,arg2,arg3
           real(kind=sp) :: t0,t1,t2,t3,t4
           real(kind=sp) :: kom0
           integer(kind=i4) :: i,m,m1
           kom0 = k*om0
           m = mod(n,4)
           if(m /= 0) then
              do i=1, m
                 t0        = real(i,kind=sp)
                 arg0      = kom0*t0
                 sins(i)   = sin(arg0)
              end do
              if(n<4) return
           end if
           m1 = m+1
            !dir$ assume_aligned coss:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
           do i=m1,n,4
              t0        = real(i+0,kind=sp)
              arg0      = kom0*t0
              sins(i+0) = sin(arg0)
              t1        = real(i+1,kind=sp)
              arg1      = kom0*t1
              sins(i+1) = sin(arg1)
              t2        = real(i+2,kind=sp)
              arg2      = kom0*t2
              sins(i+2) = sin(arg2)
              t3        = real(i+3,kind=sp)
              arg3      = kom0*t3
              sins(i+3) = sin(arg3)
           end do
           
       end subroutine sin_series_unroll_4x_r4


       subroutine sin_series_unroll_4x_r8(om0,n,sins,k)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  sin_series_unroll_8x_r8
           !dir$ attributes forceinline ::  sin_series_unroll_8x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: sin_series_unroll_8x_r8
           real(kind=dp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp), dimension(1:n), intent(out) :: sins
           real(kind=dp),                 intent(in)  :: k
           real(kind=dp) :: arg0,arg1,arg2,arg3
           real(kind=dp) :: t0,t1,t2,t3,t4
           real(kind=dp) :: kom0
           integer(kind=i4) :: i,m,m1
           kom0 = k*om0
           m = mod(n,4)
           if(m /= 0) then
              do i=1, m
                 t0        = real(i,kind=dp)
                 arg0      = kom0*t0
                 sins(i)   = sin(arg0)
              end do
              if(n<4) return
           end if
           m1 = m+1
            !dir$ assume_aligned coss:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
           do i=1m,n,4
              t0        = real(i+0,kind=dp)
              arg0      = kom0*t0
              sins(i+0) = sin(arg0)
              t1        = real(i+1,kind=dp)
              arg1      = kom0*t1
              sins(i+1) = sin(arg1)
              t2        = real(i+2,kind=dp)
              arg2      = kom0*t2
              sins(i+2) = sin(arg2)
              t3        = real(i+3,kind=dp)
              arg3      = kom0*t3
              sins(i+3) = sin(arg3)
           end do
           
       end subroutine sin_series_unroll_4x_r8


       
       subroutine sin_series_unroll_2x_r4(om0,n,sins,k)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  sin_series_unroll_2x_r4
           !dir$ attributes forceinline ::  sin_series_unroll_2x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: sin_series_unroll_2x_r4
           real(kind=sp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp), dimension(1:n), intent(out) :: sins
           real(kind=sp),                 intent(in)  :: k
           real(kind=sp) :: arg0,arg1
           real(kind=sp) :: t0,t1,t2
           real(kind=sp) :: kom0
           integer(kind=i4) :: i,m,m1
           kom0 = k*om0
           m = mod(n,2)
           if(m /= 0) then
              do i=1, m
                 t0        = real(i,kind=sp)
                 arg0      = kom0*t0
                 sins(i)   = sin(arg0)
              end do
              if(n<2) return
           end if
           m1 = m+1
            !dir$ assume_aligned coss:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
           do i=m1,n,2
              t0        = real(i+0,kind=sp)
              arg0      = kom0*t0
              sins(i+0) = sin(arg0)
              t1        = real(i+1,kind=sp)
              arg1      = kom0*t1
              sins(i+1) = sin(arg1)
             
           end do
           
       end subroutine sin_series_unroll_2x_r4


       subroutine sin_series_r4(om0,n,sins,k)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  sin_series_r4
           !dir$ attributes forceinline ::  sin_series_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: sin_series_r4
           real(kind=sp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp), dimension(1:n), intent(out) :: sins
           real(kind=sp),                 intent(in)  :: k
           real(kind=sp) :: arg0,
           real(kind=sp) :: t0
           real(kind=sp) :: kom0
           integer(kind=i4) :: i
           kom0 = k*om0
          
            !dir$ assume_aligned coss:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
           do i=1,n
              t0        = real(i+0,kind=sp)
              arg0      = kom0*t0
              sins(i+0) = sin(arg0)
                        
           end do
           
       end subroutine sin_series_r4


       subroutine sin_series_unroll_2x_r8(om0,n,sins,k)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  sin_series_unroll_2x_r8
           !dir$ attributes forceinline ::  sin_series_unroll_2x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: sin_series_unroll_2x_r8
           real(kind=dp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp), dimension(1:n), intent(out) :: sins
           real(kind=dp),                 intent(in)  :: k
           real(kind=dp) :: arg0,arg1
           real(kind=dp) :: t0,t1,t2
           real(kind=dp) :: kom0
           integer(kind=i4) :: i,m,m1
           kom0 = k*om0
           m = mod(n,2)
           if(m /= 0) then
              do i=1, m
                 t0        = real(i,kind=dp)
                 arg0      = kom0*t0
                 sins(i)   = sin(arg0)
              end do
              if(n<2) return
           end if
           m1 = m+1
            !dir$ assume_aligned coss:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
           do i=m1,n,2
              t0        = real(i+0,kind=dp)
              arg0      = kom0*t0
              sins(i+0) = sin(arg0)
              t1        = real(i+1,kind=dp)
              arg1      = kom0*t1
              sins(i+1) = sin(arg1)
             
           end do
           
       end subroutine sin_series_unroll_2x_r8


       subroutine sin_series_r8(om0,n,sins,k)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  sin_series_r8
           !dir$ attributes forceinline ::  sin_series_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: sin_series_r8
           real(kind=dp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp), dimension(1:n), intent(out) :: sins
           real(kind=dp),                 intent(in)  :: k
           real(kind=dp) :: arg0,
           real(kind=dp) :: t0
           real(kind=dp) :: kom0
           integer(kind=i4) :: i
           kom0 = k*om0
          
            !dir$ assume_aligned coss:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
           do i=1,n
              t0        = real(i+0,kind=dp)
              arg0      = kom0*t0
              sins(i+0) = sin(arg0)
                        
           end do
           
       end subroutine sin_series_r8


       subroutine sin_series_exec_r4(om0,n,sins,k,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  sin_series_exec_r4
           real(kind=sp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp), dimension(1:n), intent(out) :: sins
           real(kind=sp),                 intent(in)  :: k
           integer(kind=i4),              intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                call sin_series_unroll_16x_r4(om0,n,sins,k)
              case (8)
                call sin_series_unroll_8x_r4(om0,n,sins,k)
              case (4)
                call sin_series_unroll_4x_r4(om0,n,sins,k)
              case (2)
                call sin_series_unroll_2x_r4(om0,n,sins,k)
              case (0)
                call sin_series_r4(om0,n,sins,k)
              case default
                return
            end select
       end subroutine sin_series_exec_r4


       subroutine sin_series_exec_r8(om0,n,sins,k,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  sin_series_exec_r8
           real(kind=dp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp), dimension(1:n), intent(out) :: sins
           real(kind=dp),                 intent(in)  :: k
           integer(kind=i4),              intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                call sin_series_unroll_16x_r8(om0,n,sins,k)
              case (8)
                call sin_series_unroll_8x_r8(om0,n,sins,k)
              case (4)
                call sin_series_unroll_4x_r8(om0,n,sins,k)
              case (2)
                call sin_series_unroll_2x_r8(om0,n,sins,k)
              case (0)
                call sin_series_r8(om0,n,sins,k)
              case default
                return
            end select
       end subroutine sin_series_exec_r8
     

         
       !Спектр модулированного потока излучения можно вычислить
       !с помощью прямого преобразования Фурье
       ! Last formula, p. 181
       subroutine radiation_flux_spectrum(Phi_in,Phi_out,dim_len,data_len,status)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  radiation_flux_spectrum
           use mkl_dfti
           use mkl_fft_wrappers, only : create_desc_r_c_1D,  &
                                        exec_fft_r_c_1D
           real(kind=dp),    dimension(data_len),  intent(in)  :: Phi_in
           complex(kind=sp), dimension(data_len),  intent(out) :: Phi_out
           integer(kind=i4),                       intent(in)  :: dim_len
           integer(kind=i4),                       intent(in)  :: data_len
           integer(kind=i4),                       intent(out) :: status
           type(DFTI_DESCRIPTOR), pointer :: handle
           logical(kind=i4), automatic :: callstack
           callstack = .true.
           call create_desc_r_c_1D(handle,dim_len,data_len,callstack,status)
           if(0 == status) then
              call exec_fft_r_c_1D(handle,Phi_in,Phi_out,data_len,1,callstack,status)
           end if
        end subroutine radiation_flux_spectrum


        ! форма импульса потока излучения описывается,
        ! например, косинус-квадратной зависимостью
        ! Formula 3, p. 184
        subroutine squared_cos_flux_unroll_16x_r4(Phi0t,Phi0,n,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  squared_cos_flux_unroll_16x_r4
           !dir$ attributes forceinline ::   squared_cos_flux_unroll_16x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  squared_cos_flux_unroll_16x_r4
           
           real(kind=sp), dimension(1:n),  intent(out) :: Phi0t
           real(kind=sp),                  intent(in)  :: Phi0 !radiation flux of constant intensity
           integer(kind=i4),               intent(in)  :: n
           real(kind=sp),                  intent(in)  :: tin  ! pulse length
           integer(kind=i4) :: i,m,m1
           real(kind=sp), parameter :: hpi = 1.57079632679489661923132169164_sp
           real(kind=sp), automatic :: tin2,t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=sp), automatic :: t8,t9,t10,t11,t12,t13,t14,t15
           real(kind=sp), automatic :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           real(kind=sp), automatic :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           real(kind=sp), automatic :: c0,c1,c2,c3,c4,c5,c6,c7
           real(kind=sp), automatic :: c8,c9,c10,c11,c12,c13,c14,c15
           tin2 = 0.5_sp*tin
           m    = mod(n,16)
           if(m /= 0) then
              do i=1, m
                 t0         = real(i+0,kind=sp)
                 arg0       = hpi*t0/tin2
                 c0         = cos(arg0)
                 Phi0t(i+0) = c0*c0
              end do
              if(n<16) return
            end if
            m1 = m+1
           !dir$ assume_aligned Phi0t:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,16
              t0         = real(i+0,kind=sp)
              arg0       = hpi*t0/tin2
              c0         = cos(arg0)
              Phi0t(i+0) = c0*c0
              t1         = real(i+1,kind=sp)
              arg1       = hpi*t1/tin2
              c1         = cos(arg1)
              Phi0t(i+1) = c1*c1
              t2         = real(i+2,kind=sp)
              arg2       = hpi*t2/tin2
              c2         = cos(arg2)
              Phi0t(i+2) = c2*c2
              t3         = real(i+3,kind=sp)
              arg3       = hpi*t3/tin2
              c3         = cos(arg3)
              Phi0t(i+3) = c3*c3
              t4         = real(i+4,kind=sp)
              arg4       = hpi*t4/tin2
              c4         = cos(arg4)
              Phi0t(i+4) = c4*c4
              t5         = real(i+5,kind=sp)
              arg5       = hpi*t5/tin2
              c5         = cos(arg5)
              Phi0t(i+5) = c5*c5
              t6         = real(i+6,kind=sp)
              arg6       = hpi*t6/tin2
              c6         = cos(arg6)
              Phi0t(i+6) = c6*c6
              t7         = real(i+7,kind=sp)
              arg7       = hpi*t7/tin2
              c7         = cos(arg7)
              Phi0t(i+7) = c7*c7
              t8         = real(i+8,kind=sp)
              arg8       = hpi*t8/tin2
              c8         = cos(arg8)
              Phi0t(i+8) = c8*c8
              t9         = real(i+9,kind=sp)
              arg9       = hpi*t9/tin2
              c9         = cos(arg9)
              Phi0t(i+9) = c9*c9
              t10        = real(i+10,kind=sp)
              arg10      = hpi*t10/tin2
              c10        = cos(arg10)
              Phi0t(i+10)= c10*c10
              t11        = real(i+11,kind=sp)
              arg11      = hpi*t11/tin2
              c11        = cos(arg11)
              Phi0t(i+11)= c11*c11
              t12        = real(i+12,kind=sp)
              arg12      = hpi*t12/tin2
              c12        = cos(arg12)
              Phi0t(i+12)= c12*c12
              t13        = real(i+13,kind=sp)
              arg13      = hpi*t13/tin2
              c13        = cos(arg13)
              Phi0t(i+13)= c13*c13
              t14        = real(i+14,kind=sp)
              arg14      = hpi*t14/tin2
              c14        = cos(arg14)
              Phi0t(i+14)= c14*c14
              t15        = real(i+15,kind=sp)
              arg15      = hpi*t15/tin2
              c15        = cos(arg15)
              Phi0t(i+15)= c15*c15
           end do
         
        end subroutine squared_cos_flux_unroll_16x_r4


        subroutine squared_cos_flux_unroll_16x_r8(Phi0t,Phi0,n,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  squared_cos_flux_unroll_16x_r8
           !dir$ attributes forceinline ::   squared_cos_flux_unroll_16x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  squared_cos_flux_unroll_16x_r8
           
           real(kind=dp), dimension(1:n),  intent(out) :: Phi0t
           real(kind=dp),                  intent(in)  :: Phi0 !radiation flux of constant intensity
           integer(kind=i4),               intent(in)  :: n
           real(kind=dp),                  intent(in)  :: tin  ! pulse length
           integer(kind=i4) :: i,m,m1
           real(kind=dp), parameter :: hpi = 1.57079632679489661923132169164_dp
           real(kind=dp), automatic :: tin2,t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=dp), automatic :: t8,t9,t10,t11,t12,t13,t14,t15
           real(kind=dp), automatic :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           real(kind=dp), automatic :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           real(kind=dp), automatic :: c0,c1,c2,c3,c4,c5,c6,c7
           real(kind=dp), automatic :: c8,c9,c10,c11,c12,c13,c14,c15
           tin2 = 0.5_dp*tin
           m    = mod(n,16)
           if(m /= 0) then
              do i=1, m
                 t0         = real(i+0,kind=dp)
                 arg0       = hpi*t0/tin2
                 c0         = cos(arg0)
                 Phi0t(i+0) = c0*c0
              end do
              if(n<16) return
            end if
            m1 = m+1
           !dir$ assume_aligned Phi0t:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,16
              t0         = real(i+0,kind=dp)
              arg0       = hpi*t0/tin2
              c0         = cos(arg0)
              Phi0t(i+0) = c0*c0
              t1         = real(i+1,kind=dp)
              arg1       = hpi*t1/tin2
              c1         = cos(arg1)
              Phi0t(i+1) = c1*c1
              t2         = real(i+2,kind=dp)
              arg2       = hpi*t2/tin2
              c2         = cos(arg2)
              Phi0t(i+2) = c2*c2
              t3         = real(i+3,kind=dp)
              arg3       = hpi*t3/tin2
              c3         = cos(arg3)
              Phi0t(i+3) = c3*c3
              t4         = real(i+4,kind=dp)
              arg4       = hpi*t4/tin2
              c4         = cos(arg4)
              Phi0t(i+4) = c4*c4
              t5         = real(i+5,kind=dp)
              arg5       = hpi*t5/tin2
              c5         = cos(arg5)
              Phi0t(i+5) = c5*c5
              t6         = real(i+6,kind=dp)
              arg6       = hpi*t6/tin2
              c6         = cos(arg6)
              Phi0t(i+6) = c6*c6
              t7         = real(i+7,kind=dp)
              arg7       = hpi*t7/tin2
              c7         = cos(arg7)
              Phi0t(i+7) = c7*c7
              t8         = real(i+8,kind=dp)
              arg8       = hpi*t8/tin2
              c8         = cos(arg8)
              Phi0t(i+8) = c8*c8
              t9         = real(i+9,kind=dp)
              arg9       = hpi*t9/tin2
              c9         = cos(arg9)
              Phi0t(i+9) = c9*c9
              t10        = real(i+10,kind=dp)
              arg10      = hpi*t10/tin2
              c10        = cos(arg10)
              Phi0t(i+10)= c10*c10
              t11        = real(i+11,kind=dp)
              arg11      = hpi*t11/tin2
              c11        = cos(arg11)
              Phi0t(i+11)= c11*c11
              t12        = real(i+12,kind=sp)
              arg12      = hpi*t12/tin2
              c12        = cos(arg12)
              Phi0t(i+12)= c12*c12
              t13        = real(i+13,kind=dp)
              arg13      = hpi*t13/tin2
              c13        = cos(arg13)
              Phi0t(i+13)= c13*c13
              t14        = real(i+14,kind=dp)
              arg14      = hpi*t14/tin2
              c14        = cos(arg14)
              Phi0t(i+14)= c14*c14
              t15        = real(i+15,kind=dp)
              arg15      = hpi*t15/tin2
              c15        = cos(arg15)
              Phi0t(i+15)= c15*c15
           end do
         
        end subroutine squared_cos_flux_unroll_16x_r8


       subroutine squared_cos_flux_unroll_8x_r4(Phi0t,Phi0,n,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  squared_cos_flux_unroll_8x_r4
           !dir$ attributes forceinline ::   squared_cos_flux_unroll_8x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  squared_cos_flux_unroll_8x_r4
           
           real(kind=sp), dimension(1:n),  intent(out) :: Phi0t
           real(kind=sp),                  intent(in)  :: Phi0 !radiation flux of constant intensity
           integer(kind=i4),               intent(in)  :: n
           real(kind=sp),                  intent(in)  :: tin  ! pulse length
           integer(kind=i4) :: i,m,m1
           real(kind=sp), parameter :: hpi = 1.57079632679489661923132169164_sp
           real(kind=sp), automatic :: tin2,t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=sp), automatic :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           real(kind=sp), automatic :: c0,c1,c2,c3,c4,c5,c6,c7
          
           tin2 = 0.5_sp*tin
           m    = mod(n,8)
           if(m /= 0) then
              do i=1, m
                 t0         = real(i+0,kind=sp)
                 arg0       = hpi*t0/tin2
                 c0         = cos(arg0)
                 Phi0t(i+0) = c0*c0
              end do
              if(n<8) return
            end if
            m1 = m+1
           !dir$ assume_aligned Phi0t:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,8
              t0         = real(i+0,kind=sp)
              arg0       = hpi*t0/tin2
              c0         = cos(arg0)
              Phi0t(i+0) = c0*c0
              t1         = real(i+1,kind=sp)
              arg1       = hpi*t1/tin2
              c1         = cos(arg1)
              Phi0t(i+1) = c1*c1
              t2         = real(i+2,kind=sp)
              arg2       = hpi*t2/tin2
              c2         = cos(arg2)
              Phi0t(i+2) = c2*c2
              t3         = real(i+3,kind=sp)
              arg3       = hpi*t3/tin2
              c3         = cos(arg3)
              Phi0t(i+3) = c3*c3
              t4         = real(i+4,kind=sp)
              arg4       = hpi*t4/tin2
              c4         = cos(arg4)
              Phi0t(i+4) = c4*c4
              t5         = real(i+5,kind=sp)
              arg5       = hpi*t5/tin2
              c5         = cos(arg5)
              Phi0t(i+5) = c5*c5
              t6         = real(i+6,kind=sp)
              arg6       = hpi*t6/tin2
              c6         = cos(arg6)
              Phi0t(i+6) = c6*c6
              t7         = real(i+7,kind=sp)
              arg7       = hpi*t7/tin2
              c7         = cos(arg7)
              Phi0t(i+7) = c7*c7
          end do
         
        end subroutine squared_cos_flux_unroll_8x_r4


        
        subroutine squared_cos_flux_unroll_8x_r8(Phi0t,Phi0,n,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  squared_cos_flux_unroll_8x_r8
           !dir$ attributes forceinline ::   squared_cos_flux_unroll_8x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  squared_cos_flux_unroll_8x_r8
           
           real(kind=dp), dimension(1:n),  intent(out) :: Phi0t
           real(kind=dp),                  intent(in)  :: Phi0 !radiation flux of constant intensity
           integer(kind=i4),               intent(in)  :: n
           real(kind=dp),                  intent(in)  :: tin  ! pulse length
           integer(kind=i4) :: i,m,m1
           real(kind=dp), parameter :: hpi = 1.57079632679489661923132169164_dp
           real(kind=dp), automatic :: tin2,t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=dp), automatic :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           real(kind=dp), automatic :: c0,c1,c2,c3,c4,c5,c6,c7
          
           tin2 = 0.5_dp*tin
           m    = mod(n,8)
           if(m /= 0) then
              do i=1, m
                 t0         = real(i+0,kind=dp)
                 arg0       = hpi*t0/tin2
                 c0         = cos(arg0)
                 Phi0t(i+0) = c0*c0
              end do
              if(n<8) return
            end if
            m1 = m+1
           !dir$ assume_aligned Phi0t:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,8
              t0         = real(i+0,kind=dp)
              arg0       = hpi*t0/tin2
              c0         = cos(arg0)
              Phi0t(i+0) = c0*c0
              t1         = real(i+1,kind=dp)
              arg1       = hpi*t1/tin2
              c1         = cos(arg1)
              Phi0t(i+1) = c1*c1
              t2         = real(i+2,kind=dp)
              arg2       = hpi*t2/tin2
              c2         = cos(arg2)
              Phi0t(i+2) = c2*c2
              t3         = real(i+3,kind=dp)
              arg3       = hpi*t3/tin2
              c3         = cos(arg3)
              Phi0t(i+3) = c3*c3
              t4         = real(i+4,kind=dp)
              arg4       = hpi*t4/tin2
              c4         = cos(arg4)
              Phi0t(i+4) = c4*c4
              t5         = real(i+5,kind=dp)
              arg5       = hpi*t5/tin2
              c5         = cos(arg5)
              Phi0t(i+5) = c5*c5
              t6         = real(i+6,kind=dp)
              arg6       = hpi*t6/tin2
              c6         = cos(arg6)
              Phi0t(i+6) = c6*c6
              t7         = real(i+7,kind=dp)
              arg7       = hpi*t7/tin2
              c7         = cos(arg7)
              Phi0t(i+7) = c7*c7
          end do
         
        end subroutine squared_cos_flux_unroll_8x_r8



        subroutine squared_cos_flux_unroll_4x_r4(Phi0t,Phi0,n,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  squared_cos_flux_unroll_4x_r4
           !dir$ attributes forceinline ::   squared_cos_flux_unroll_4x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  squared_cos_flux_unroll_4x_r4
           
           real(kind=sp), dimension(1:n),  intent(out) :: Phi0t
           real(kind=sp),                  intent(in)  :: Phi0 !radiation flux of constant intensity
           integer(kind=i4),               intent(in)  :: n
           real(kind=sp),                  intent(in)  :: tin  ! pulse length
           integer(kind=i4) :: i,m,m1
           real(kind=sp), parameter :: hpi = 1.57079632679489661923132169164_sp
           real(kind=sp), automatic :: tin2,t0,t1,t2,t3
           real(kind=sp), automatic :: arg0,arg1,arg2,arg3
           real(kind=sp), automatic :: c0,c1,c2,c3
          
           tin2 = 0.5_sp*tin
           m    = mod(n,4)
           if(m /= 0) then
              do i=1, m
                 t0         = real(i+0,kind=sp)
                 arg0       = hpi*t0/tin2
                 c0         = cos(arg0)
                 Phi0t(i+0) = c0*c0
              end do
              if(n<4) return
            end if
            m1 = m+1
           !dir$ assume_aligned Phi0t:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,4
              t0         = real(i+0,kind=sp)
              arg0       = hpi*t0/tin2
              c0         = cos(arg0)
              Phi0t(i+0) = c0*c0
              t1         = real(i+1,kind=sp)
              arg1       = hpi*t1/tin2
              c1         = cos(arg1)
              Phi0t(i+1) = c1*c1
              t2         = real(i+2,kind=sp)
              arg2       = hpi*t2/tin2
              c2         = cos(arg2)
              Phi0t(i+2) = c2*c2
              t3         = real(i+3,kind=sp)
              arg3       = hpi*t3/tin2
              c3         = cos(arg3)
              Phi0t(i+3) = c3*c3
           end do
         
        end subroutine squared_cos_flux_unroll_4x_r4


       subroutine squared_cos_flux_unroll_4x_r8(Phi0t,Phi0,n,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  squared_cos_flux_unroll_4x_r8
           !dir$ attributes forceinline ::   squared_cos_flux_unroll_4x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  squared_cos_flux_unroll_4x_r8
           
           real(kind=dp), dimension(1:n),  intent(out) :: Phi0t
           real(kind=dp),                  intent(in)  :: Phi0 !radiation flux of constant intensity
           integer(kind=i4),               intent(in)  :: n
           real(kind=dp),                  intent(in)  :: tin  ! pulse length
           integer(kind=i4) :: i,m,m1
           real(kind=dp), parameter :: hpi = 1.57079632679489661923132169164_dp
           real(kind=dp), automatic :: tin2,t0,t1,t2,t3
           real(kind=dp), automatic :: arg0,arg1,arg2,arg3
           real(kind=dp), automatic :: c0,c1,c2,c3
          
           tin2 = 0.5_dp*tin
           m    = mod(n,4)
           if(m /= 0) then
              do i=1, m
                 t0         = real(i+0,kind=dp)
                 arg0       = hpi*t0/tin2
                 c0         = cos(arg0)
                 Phi0t(i+0) = c0*c0
              end do
              if(n<4) return
            end if
            m1 = m+1
           !dir$ assume_aligned Phi0t:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,4
              t0         = real(i+0,kind=dp)
              arg0       = hpi*t0/tin2
              c0         = cos(arg0)
              Phi0t(i+0) = c0*c0
              t1         = real(i+1,kind=dp)
              arg1       = hpi*t1/tin2
              c1         = cos(arg1)
              Phi0t(i+1) = c1*c1
              t2         = real(i+2,kind=dp)
              arg2       = hpi*t2/tin2
              c2         = cos(arg2)
              Phi0t(i+2) = c2*c2
              t3         = real(i+3,kind=dp)
              arg3       = hpi*t3/tin2
              c3         = cos(arg3)
              Phi0t(i+3) = c3*c3
           end do
         
        end subroutine squared_cos_flux_unroll_4x_r8


        
       subroutine squared_cos_flux_unroll_2x_r4(Phi0t,Phi0,n,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  squared_cos_flux_unroll_2x_r4
           !dir$ attributes forceinline ::   squared_cos_flux_unroll_2x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  squared_cos_flux_unroll_2x_r4
           
           real(kind=sp), dimension(1:n),  intent(out) :: Phi0t
           real(kind=sp),                  intent(in)  :: Phi0 !radiation flux of constant intensity
           integer(kind=i4),               intent(in)  :: n
           real(kind=sp),                  intent(in)  :: tin  ! pulse length
           integer(kind=i4) :: i,m,m1
           real(kind=sp), parameter :: hpi = 1.57079632679489661923132169164_sp
           real(kind=sp), automatic :: tin2,t0,t1
           real(kind=sp), automatic :: arg0,arg1
           real(kind=sp), automatic :: c0,c1
          
           tin2 = 0.5_sp*tin
           m    = mod(n,2)
           if(m /= 0) then
              do i=1, m
                 t0         = real(i+0,kind=sp)
                 arg0       = hpi*t0/tin2
                 c0         = cos(arg0)
                 Phi0t(i+0) = c0*c0
              end do
              if(n<2) return
            end if
            m1 = m+1
           !dir$ assume_aligned Phi0t:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,2
              t0         = real(i+0,kind=sp)
              arg0       = hpi*t0/tin2
              c0         = cos(arg0)
              Phi0t(i+0) = c0*c0
              t1         = real(i+1,kind=sp)
              arg1       = hpi*t1/tin2
              c1         = cos(arg1)
              Phi0t(i+1) = c1*c1
           end do
         
        end subroutine squared_cos_flux_unroll_2x_r4


        
        subroutine squared_cos_flux_unroll_2x_r8(Phi0t,Phi0,n,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  squared_cos_flux_unroll_2x_r8
           !dir$ attributes forceinline ::   squared_cos_flux_unroll_2x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  squared_cos_flux_unroll_2x_r8
           
           real(kind=dp), dimension(1:n),  intent(out) :: Phi0t
           real(kind=dp),                  intent(in)  :: Phi0 !radiation flux of constant intensity
           integer(kind=i4),               intent(in)  :: n
           real(kind=dp),                  intent(in)  :: tin  ! pulse length
           integer(kind=i4) :: i,m,m1
           real(kind=dp), parameter :: hpi = 1.57079632679489661923132169164_dp
           real(kind=dp), automatic :: tin2,t0,t1
           real(kind=dp), automatic :: arg0,arg1
           real(kind=dp), automatic :: c0,c1
          
           tin2 = 0.5_dp*tin
           m    = mod(n,2)
           if(m /= 0) then
              do i=1, m
                 t0         = real(i+0,kind=dp)
                 arg0       = hpi*t0/tin2
                 c0         = cos(arg0)
                 Phi0t(i+0) = c0*c0
              end do
              if(n<2) return
            end if
            m1 = m+1
           !dir$ assume_aligned Phi0t:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,2
              t0         = real(i+0,kind=dp)
              arg0       = hpi*t0/tin2
              c0         = cos(arg0)
              Phi0t(i+0) = c0*c0
              t1         = real(i+1,kind=dp)
              arg1       = hpi*t1/tin2
              c1         = cos(arg1)
              Phi0t(i+1) = c1*c1
           end do
         
        end subroutine squared_cos_flux_unroll_2x_r8


        subroutine squared_cos_flux_r8(Phi0t,Phi0,n,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  squared_cos_flux_r8
           !dir$ attributes forceinline ::   squared_cos_flux_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  squared_cos_flux_r8
           
           real(kind=dp), dimension(1:n),  intent(out) :: Phi0t
           real(kind=dp),                  intent(in)  :: Phi0 !radiation flux of constant intensity
           integer(kind=i4),               intent(in)  :: n
           real(kind=dp),                  intent(in)  :: tin  ! pulse length
           integer(kind=i4) :: i,m,m1
           real(kind=dp), parameter :: hpi = 1.57079632679489661923132169164_dp
           real(kind=dp), automatic :: tin2,t0
           real(kind=dp), automatic :: arg0
           real(kind=dp), automatic :: c0
          
           tin2 = 0.5_dp*tin
          
           !dir$ assume_aligned Phi0t:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=1,n
              t0         = real(i+0,kind=dp)
              arg0       = hpi*t0/tin2
              c0         = cos(arg0)
              Phi0t(i+0) = c0*c0
             
           end do
         
        end subroutine squared_cos_flux_r8


        subroutine squared_cos_flux_r4(Phi0t,Phi0,n,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  squared_cos_flux_r4
           !dir$ attributes forceinline ::   squared_cos_flux_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  squared_cos_flux_r4
           
           real(kind=sp), dimension(1:n),  intent(out) :: Phi0t
           real(kind=sp),                  intent(in)  :: Phi0 !radiation flux of constant intensity
           integer(kind=i4),               intent(in)  :: n
           real(kind=sp),                  intent(in)  :: tin  ! pulse length
           integer(kind=i4) :: i,m,m1
           real(kind=sp), parameter :: hpi = 1.57079632679489661923132169164_sp
           real(kind=sp), automatic :: tin2,t0
           real(kind=sp), automatic :: arg0
           real(kind=sp), automatic :: c0
          
           tin2 = 0.5_sp*tin
          
           !dir$ assume_aligned Phi0t:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=1,n
              t0         = real(i+0,kind=sp)
              arg0       = hpi*t0/tin2
              c0         = cos(arg0)
              Phi0t(i+0) = c0*c0
             
           end do
         
        end subroutine squared_cos_flux_r4



        subroutine squared_cos_flux_exec_r4(Phi0t,Phi0,n,tin,unroll_cnt)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 ::  squared_cos_flux_exec_r4
           real(kind=sp), dimension(1:n),  intent(out) :: Phi0t
           real(kind=sp),                  intent(in)  :: Phi0 !radiation flux of constant intensity
           integer(kind=i4),               intent(in)  :: n
           real(kind=sp),                  intent(in)  :: tin  ! pulse length
           integer(kind=i4),               intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                call squared_cos_flux_unroll_16x_r4(Phi0t,Phi0,n,tin)
              case (8)
                call squared_cos_flux_unroll_8x_r4(Phi0t,Phi0,n,tin)
              case (4)
                call squared_cos_flux_unroll_4x_r4(Phi0t,Phi0,n,tin)
              case (2)
                call squared_cos_flux_unroll_2x_r4(Phi0t,Phi0,n,tin)
              case (0)
                call squared_cos_flux_r4(Phi0t,Phi0,n,tin)
              case default
                return
           end select
        end subroutine squared_cos_flux_exec_r4


        subroutine squared_cos_flux_exec_r8(Phi0t,Phi0,n,tin,unroll_cnt)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 ::  squared_cos_flux_exec_r8
           real(kind=dp), dimension(1:n),  intent(out) :: Phi0t
           real(kind=dp),                  intent(in)  :: Phi0 !radiation flux of constant intensity
           integer(kind=i4),               intent(in)  :: n
           real(kind=dp),                  intent(in)  :: tin  ! pulse length
           integer(kind=i4),               intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                call squared_cos_flux_unroll_16x_r8(Phi0t,Phi0,n,tin)
              case (8)
                call squared_cos_flux_unroll_8x_r8(Phi0t,Phi0,n,tin)
              case (4)
                call squared_cos_flux_unroll_4x_r8(Phi0t,Phi0,n,tin)
              case (2)
                call squared_cos_flux_unroll_2x_r8(Phi0t,Phi0,n,tin)
              case (0)
                call squared_cos_flux_r8(Phi0t,Phi0,n,tin)
              case default
                return
           end select
        end subroutine squared_cos_flux_exec_r8

       
       


        subroutine const_flux_spectr_unroll_16x_r4(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_16x_r4
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_16x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_16x_r4
           real(kind=sp),  dimension(1:n), intent(out) :: Phi0f
           real(kind=sp),                  intent(in)  :: Phi0
           real(kind=sp),  dimension(1:n), intent(in)  :: freq
           integer(kind=i4),               intent(in)  :: n
           real(kind=sp),                  intent(in)  :: T
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           real(kind=sp) :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           real(kind=sp) :: f0,f1,f2,f3,f4,f5,f6,f7
           real(kind=sp) :: f8,f9,f10,f11,f12,f13,f14,f15
           real(kind=sp) :: c0,c1,c2,c3,c4,c5,c6,c7
           real(kind=sp) :: c8,c9,c10,c11,c12,c13,c14,c15
           real(kind=sp) :: sa0,sa1,sa2,sa3,sa4,sa,sa6,sa7
           real(kind=sp) :: sa8,sa9,sa10,sa11,sa12,sa13,sa14,sa15
           real(kind=sp), automatic :: hT,Phi0T
           integer(kind=i4) :: i,m,m1
           hT    = 0.5_sp*T
           Phi0T = Phi0*hT
           m = mod(n,16)
           if(m /= 0) then
              do i=1, m
                 f0         = freq(i+0)
                 c0         = twopi*f0*hT
                 sa0        = sin(c0)/c0
                 arg0       = 1.0_sp-(f0*hT*f0*hT)
                 Phi0f(i+0) = Phi0T*(sa0/arg0)
              end do
              if(n<16) return
           end if
           m1 = m+1
           !dir$ assume_aligned Phi0f:64
           !dir$ assume_aligned freq:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,16
              f0         = freq(i+0)
              c0         = twopi*f0*hT
              sa0        = sin(c0)/c0
              arg0       = 1.0_sp-(f0*hT*f0*hT)
              Phi0f(i+0) = Phi0T*(sa0/arg0)
              f1         = freq(i+1)
              c1         = twopi*f1*hT
              sa1        = sin(c1)/c1
              arg1       = 1.0_sp-(f1*hT*f1*hT)
              Phi0f(i+1) = Phi0T*(sa1/arg1)
              f2         = freq(i+2)
              c2         = twopi*f2*hT
              sa2        = sin(c2)/c2
              arg2       = 1.0_sp-(f2*hT*f2*hT)
              Phi0f(i+2) = Phi0T*(sa2/arg2)
              f3         = freq(i+3)
              c3         = twopi*f3*hT
              sa3        = sin(c3)/c3
              arg3       = 1.0_sp-(f3*hT*f3*hT)
              Phi0f(i+3) = Phi0T*(sa3/arg3)
              f4         = freq(i+4)
              c4         = twopi*f4*hT
              sa4        = sin(c4)/c4
              arg4       = 1.0_sp-(f4*hT*f4*hT)
              Phi0f(i+4) = Phi0T*(sa4/arg4)
              f5         = freq(i+5)
              c5         = twopi*f5*hT
              sa5        = sin(c5)/c5
              arg5       = 1.0_sp-(f5*hT*f5*hT)
              Phi0f(i+5) = Phi0T*(sa5/arg5)
              f6         = freq(i+6)
              c6         = twopi*f6*hT
              sa6        = sin(c6)/c6
              arg6       = 1.0_sp-(f6*hT*f6*hT)
              Phi0f(i+6) = Phi0T*(sa6/arg6)
              f7         = freq(i+7)
              c7         = twopi*f7*hT
              sa7        = sin(c7)/c7
              arg7       = 1.0_sp-(f7*hT*f7*hT)
              Phi0f(i+7) = Phi0T*(sa7/arg7)
              f8         = freq(i+8)
              c8         = twopi*f8*hT
              sa8        = sin(c8)/c8
              arg8       = 1.0_sp-(f8*hT*f8*hT)
              Phi0f(i+8) = Phi0T*(sa8/arg8)
              f9         = freq(i+9)
              c9         = twopi*f9*hT
              sa9        = sin(c9)/c9
              arg9       = 1.0_sp-(f9*hT*f9*hT)
              Phi0f(i+9) = Phi0T*(sa9/arg9)
              f10        = freq(i+10)
              c10        = twopi*f10*hT
              sa10       = sin(c10)/c10
              arg10      = 1.0_sp-(f10*hT*f10*hT)
              Phi0f(i+10)= Phi0T*(sa10/arg10)
              f11        = freq(i+11)
              c11        = twopi*f11*hT
              sa11       = sin(c11)/c11
              arg11      = 1.0_sp-(f11*hT*f11*hT)
              Phi0f(i+11)= Phi0T*(sa11/arg11)
              f12        = freq(i+12)
              c12        = twopi*f12*hT
              sa12       = sin(c12)/c12
              arg12      = 1.0_sp-(f12*hT*f12*hT)
              Phi0f(i+12)= Phi0T*(sa12/arg12)
              f13        = freq(i+13)
              c13        = twopi*f13*hT
              sa13       = sin(c13)/c13
              arg13      = 1.0_sp-(f13*hT*f13*hT)
              Phi0f(i+13) = Phi0T*(sa13/arg13)
              f14        = freq(i+14)
              c14        = twopi*f14*hT
              sa14       = sin(c14)/c14
              arg14      = 1.0_sp-(f14*hT*f14*hT)
              Phi0f(i+14)= Phi0T*(sa14/arg14)
              f15        = freq(i+15)
              c15        = twopi*f15*hT
              sa15       = sin(c15)/c15
              arg15      = 1.0_sp-(f15*hT*f15*hT)
              Phi0f(i+15)= Phi0T*(sa15/arg15)
           end do
          
       end subroutine const_flux_spectr_unroll_16x_r4


       subroutine const_flux_spectr_unroll_8x_r4(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_8x_r4
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_8x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_8x_r4
           real(kind=sp),  dimension(1:n), intent(out) :: Phi0f
           real(kind=sp),                  intent(in)  :: Phi0
           real(kind=sp),  dimension(1:n), intent(in)  :: freq
           integer(kind=i4),               intent(in)  :: n
           real(kind=sp),                  intent(in)  :: T
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           real(kind=sp) :: f0,f1,f2,f3,f4,f5,f6,f7
           real(kind=sp) :: c0,c1,c2,c3,c4,c5,c6,c7
           real(kind=sp) :: sa0,sa1,sa2,sa3,sa4,sa,sa6,sa7
           real(kind=sp), automatic :: hT,Phi0T
           integer(kind=i4) :: i,m,m1
           hT    = 0.5_sp*T
           Phi0T = Phi0*hT
           m = mod(n,8)
           if(m /= 0) then
              do i=1, m
                 f0         = freq(i+0)
                 c0         = twopi*f0*hT
                 sa0        = sin(c0)/c0
                 arg0       = 1.0_sp-(f0*hT*f0*hT)
                 Phi0f(i+0) = Phi0T*(sa0/arg0)
              end do
              if(n<8) return
           end if
           m1 = m+1
           !dir$ assume_aligned Phi0f:64
           !dir$ assume_aligned freq:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,8
              f0         = freq(i+0)
              c0         = twopi*f0*hT
              sa0        = sin(c0)/c0
              arg0       = 1.0_sp-(f0*hT*f0*hT)
              Phi0f(i+0) = Phi0T*(sa0/arg0)
              f1         = freq(i+1)
              c1         = twopi*f1*hT
              sa1        = sin(c1)/c1
              arg1       = 1.0_sp-(f1*hT*f1*hT)
              Phi0f(i+1) = Phi0T*(sa1/arg1)
              f2         = freq(i+2)
              c2         = twopi*f2*hT
              sa2        = sin(c2)/c2
              arg2       = 1.0_sp-(f2*hT*f2*hT)
              Phi0f(i+2) = Phi0T*(sa2/arg2)
              f3         = freq(i+3)
              c3         = twopi*f3*hT
              sa3        = sin(c3)/c3
              arg3       = 1.0_sp-(f3*hT*f3*hT)
              Phi0f(i+3) = Phi0T*(sa3/arg3)
              f4         = freq(i+4)
              c4         = twopi*f4*hT
              sa4        = sin(c4)/c4
              arg4       = 1.0_sp-(f4*hT*f4*hT)
              Phi0f(i+4) = Phi0T*(sa4/arg4)
              f5         = freq(i+5)
              c5         = twopi*f5*hT
              sa5        = sin(c5)/c5
              arg5       = 1.0_sp-(f5*hT*f5*hT)
              Phi0f(i+5) = Phi0T*(sa5/arg5)
              f6         = freq(i+6)
              c6         = twopi*f6*hT
              sa6        = sin(c6)/c6
              arg6       = 1.0_sp-(f6*hT*f6*hT)
              Phi0f(i+6) = Phi0T*(sa6/arg6)
              f7         = freq(i+7)
              c7         = twopi*f7*hT
              sa7        = sin(c7)/c7
              arg7       = 1.0_sp-(f7*hT*f7*hT)
              Phi0f(i+7) = Phi0T*(sa7/arg7)
          end do
          
       end subroutine const_flux_spectr_unroll_8x_r4


       subroutine const_flux_spectr_unroll_4x_r4(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_4x_r4
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_4x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_4x_r4
           real(kind=sp),  dimension(1:n), intent(out) :: Phi0f
           real(kind=sp),                  intent(in)  :: Phi0
           real(kind=sp),  dimension(1:n), intent(in)  :: freq
           integer(kind=i4),               intent(in)  :: n
           real(kind=sp),                  intent(in)  :: T
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp) :: arg0,arg1,arg2,arg3
           real(kind=sp) :: f0,f1,f2,f3
           real(kind=sp) :: c0,c1,c2,c3
           real(kind=sp) :: sa0,sa1,sa2,sa3
           real(kind=sp), automatic :: hT,Phi0T
           integer(kind=i4) :: i,m,m1
           hT    = 0.5_sp*T
           Phi0T = Phi0*hT
           m = mod(n,4)
           if(m /= 0) then
              do i=1, m
                 f0         = freq(i+0)
                 c0         = twopi*f0*hT
                 sa0        = sin(c0)/c0
                 arg0       = 1.0_sp-(f0*hT*f0*hT)
                 Phi0f(i+0) = Phi0T*(sa0/arg0)
              end do
              if(n<4) return
           end if
           m1 = m+1
           !dir$ assume_aligned Phi0f:64
           !dir$ assume_aligned freq:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,4
              f0         = freq(i+0)
              c0         = twopi*f0*hT
              sa0        = sin(c0)/c0
              arg0       = 1.0_sp-(f0*hT*f0*hT)
              Phi0f(i+0) = Phi0T*(sa0/arg0)
              f1         = freq(i+1)
              c1         = twopi*f1*hT
              sa1        = sin(c1)/c1
              arg1       = 1.0_sp-(f1*hT*f1*hT)
              Phi0f(i+1) = Phi0T*(sa1/arg1)
              f2         = freq(i+2)
              c2         = twopi*f2*hT
              sa2        = sin(c2)/c2
              arg2       = 1.0_sp-(f2*hT*f2*hT)
              Phi0f(i+2) = Phi0T*(sa2/arg2)
              f3         = freq(i+3)
              c3         = twopi*f3*hT
              sa3        = sin(c3)/c3
              arg3       = 1.0_sp-(f3*hT*f3*hT)
              Phi0f(i+3) = Phi0T*(sa3/arg3)
           end do
          
       end subroutine const_flux_spectr_unroll_4x_r4



       subroutine const_flux_spectr_unroll_2x_r4(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_2x_r4
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_2x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_2x_r4
           real(kind=sp),  dimension(1:n), intent(out) :: Phi0f
           real(kind=sp),                  intent(in)  :: Phi0
           real(kind=sp),  dimension(1:n), intent(in)  :: freq
           integer(kind=i4),               intent(in)  :: n
           real(kind=sp),                  intent(in)  :: T
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp) :: arg0,arg1
           real(kind=sp) :: f0,f1
           real(kind=sp) :: c0,c1
           real(kind=sp) :: sa0,sa1
           real(kind=sp), automatic :: hT,Phi0T
           integer(kind=i4) :: i,m,m1
           hT    = 0.5_sp*T
           Phi0T = Phi0*hT
           m = mod(n,2)
           if(m /= 0) then
              do i=1, m
                 f0         = freq(i+0)
                 c0         = twopi*f0*hT
                 sa0        = sin(c0)/c0
                 arg0       = 1.0_sp-(f0*hT*f0*hT)
                 Phi0f(i+0) = Phi0T*(sa0/arg0)
              end do
              if(n<2) return
           end if
           m1 = m+1
           !dir$ assume_aligned Phi0f:64
           !dir$ assume_aligned freq:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,2
              f0         = freq(i+0)
              c0         = twopi*f0*hT
              sa0        = sin(c0)/c0
              arg0       = 1.0_sp-(f0*hT*f0*hT)
              Phi0f(i+0) = Phi0T*(sa0/arg0)
              f1         = freq(i+1)
              c1         = twopi*f1*hT
              sa1        = sin(c1)/c1
              arg1       = 1.0_sp-(f1*hT*f1*hT)
              Phi0f(i+1) = Phi0T*(sa1/arg1)
            end do
          
       end subroutine const_flux_spectr_unroll_2x_r4


       subroutine const_flux_spectr_r4(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_r4
           !dir$ attributes forceinline ::   const_flux_spectr_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_r4
           real(kind=sp),  dimension(1:n), intent(out) :: Phi0f
           real(kind=sp),                  intent(in)  :: Phi0
           real(kind=sp),  dimension(1:n), intent(in)  :: freq
           integer(kind=i4),               intent(in)  :: n
           real(kind=sp),                  intent(in)  :: T
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp) :: arg0
           real(kind=sp) :: f0
           real(kind=sp) :: c0
           real(kind=sp) :: sa0
           real(kind=sp), automatic :: hT,Phi0T
           integer(kind=i4) :: i
           hT    = 0.5_sp*T
           Phi0T = Phi0*hT
          
           !dir$ assume_aligned Phi0f:64
           !dir$ assume_aligned freq:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=1,n
              f0         = freq(i+0)
              c0         = twopi*f0*hT
              sa0        = sin(c0)/c0
              arg0       = 1.0_sp-(f0*hT*f0*hT)
              Phi0f(i+0) = Phi0T*(sa0/arg0)
           end do
          
       end subroutine const_flux_spectr_r4





       



       subroutine const_flux_spectr_unroll_16x_r8(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_16x_r8
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_16x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_16x_r8
           real(kind=dp),  dimension(1:n), intent(out) :: Phi0f
           real(kind=dp),                  intent(in)  :: Phi0
           real(kind=dp),  dimension(1:n), intent(in)  :: freq
           integer(kind=i4),               intent(in)  :: n
           real(kind=dp),                  intent(in)  :: T
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           real(kind=dp) :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           real(kind=dp) :: f0,f1,f2,f3,f4,f5,f6,f7
           real(kind=dp) :: f8,f9,f10,f11,f12,f13,f14,f15
           real(kind=dp) :: c0,c1,c2,c3,c4,c5,c6,c7
           real(kind=dp) :: c8,c9,c10,c11,c12,c13,c14,c15
           real(kind=dp) :: sa0,sa1,sa2,sa3,sa4,sa,sa6,sa7
           real(kind=dp) :: sa8,sa9,sa10,sa11,sa12,sa13,sa14,sa15
           real(kind=dp), automatic :: hT,Phi0T
           integer(kind=i4) :: i,m,m1
           hT    = 0.5_dp*T
           Phi0T = Phi0*hT
           m = mod(n,16)
           if(m /= 0) then
              do i=1, m
                 f0         = freq(i+0)
                 c0         = twopi*f0*hT
                 sa0        = sin(c0)/c0
                 arg0       = 1.0_dp-(f0*hT*f0*hT)
                 Phi0f(i+0) = Phi0T*(sa0/arg0)
              end do
              if(n<16) return
           end if
           m1 = m+1
           !dir$ assume_aligned Phi0f:64
           !dir$ assume_aligned freq:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,16
              f0         = freq(i+0)
              c0         = twopi*f0*hT
              sa0        = sin(c0)/c0
              arg0       = 1.0_dp-(f0*hT*f0*hT)
              Phi0f(i+0) = Phi0T*(sa0/arg0)
              f1         = freq(i+1)
              c1         = twopi*f1*hT
              sa1        = sin(c1)/c1
              arg1       = 1.0_dp-(f1*hT*f1*hT)
              Phi0f(i+1) = Phi0T*(sa1/arg1)
              f2         = freq(i+2)
              c2         = twopi*f2*hT
              sa2        = sin(c2)/c2
              arg2       = 1.0_dp-(f2*hT*f2*hT)
              Phi0f(i+2) = Phi0T*(sa2/arg2)
              f3         = freq(i+3)
              c3         = twopi*f3*hT
              sa3        = sin(c3)/c3
              arg3       = 1.0_dp-(f3*hT*f3*hT)
              Phi0f(i+3) = Phi0T*(sa3/arg3)
              f4         = freq(i+4)
              c4         = twopi*f4*hT
              sa4        = sin(c4)/c4
              arg4       = 1.0_dp-(f4*hT*f4*hT)
              Phi0f(i+4) = Phi0T*(sa4/arg4)
              f5         = freq(i+5)
              c5         = twopi*f5*hT
              sa5        = sin(c5)/c5
              arg5       = 1.0_dp-(f5*hT*f5*hT)
              Phi0f(i+5) = Phi0T*(sa5/arg5)
              f6         = freq(i+6)
              c6         = twopi*f6*hT
              sa6        = sin(c6)/c6
              arg6       = 1.0_dp-(f6*hT*f6*hT)
              Phi0f(i+6) = Phi0T*(sa6/arg6)
              f7         = freq(i+7)
              c7         = twopi*f7*hT
              sa7        = sin(c7)/c7
              arg7       = 1.0_dp-(f7*hT*f7*hT)
              Phi0f(i+7) = Phi0T*(sa7/arg7)
              f8         = freq(i+8)
              c8         = twopi*f8*hT
              sa8        = sin(c8)/c8
              arg8       = 1.0_dp-(f8*hT*f8*hT)
              Phi0f(i+8) = Phi0T*(sa8/arg8)
              f9         = freq(i+9)
              c9         = twopi*f9*hT
              sa9        = sin(c9)/c9
              arg9       = 1.0_dp-(f9*hT*f9*hT)
              Phi0f(i+9) = Phi0T*(sa9/arg9)
              f10        = freq(i+10)
              c10        = twopi*f10*hT
              sa10       = sin(c10)/c10
              arg10      = 1.0_dp-(f10*hT*f10*hT)
              Phi0f(i+10)= Phi0T*(sa10/arg10)
              f11        = freq(i+11)
              c11        = twopi*f11*hT
              sa11       = sin(c11)/c11
              arg11      = 1.0_dp-(f11*hT*f11*hT)
              Phi0f(i+11)= Phi0T*(sa11/arg11)
              f12        = freq(i+12)
              c12        = twopi*f12*hT
              sa12       = sin(c12)/c12
              arg12      = 1.0_dp-(f12*hT*f12*hT)
              Phi0f(i+12)= Phi0T*(sa12/arg12)
              f13        = freq(i+13)
              c13        = twopi*f13*hT
              sa13       = sin(c13)/c13
              arg13      = 1.0_dp-(f13*hT*f13*hT)
              Phi0f(i+13) = Phi0T*(sa13/arg13)
              f14        = freq(i+14)
              c14        = twopi*f14*hT
              sa14       = sin(c14)/c14
              arg14      = 1.0_dp-(f14*hT*f14*hT)
              Phi0f(i+14)= Phi0T*(sa14/arg14)
              f15        = freq(i+15)
              c15        = twopi*f15*hT
              sa15       = sin(c15)/c15
              arg15      = 1.0_dp-(f15*hT*f15*hT)
              Phi0f(i+15)= Phi0T*(sa15/arg15)
           end do
          
       end subroutine const_flux_spectr_unroll_16x_r8

       
       subroutine const_flux_spectr_unroll_8x_r8(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_8x_r8
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_8x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_8x_r8
           real(kind=dp),  dimension(1:n), intent(out) :: Phi0f
           real(kind=dp),                  intent(in)  :: Phi0
           real(kind=dp),  dimension(1:n), intent(in)  :: freq
           integer(kind=i4),               intent(in)  :: n
           real(kind=dp),                  intent(in)  :: T
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           real(kind=dp) :: f0,f1,f2,f3,f4,f5,f6,f7
           real(kind=dp) :: c0,c1,c2,c3,c4,c5,c6,c7
           real(kind=dp) :: sa0,sa1,sa2,sa3,sa4,sa,sa6,sa7
           real(kind=dp), automatic :: hT,Phi0T
           integer(kind=i4) :: i,m,m1
           hT    = 0.5_dp*T
           Phi0T = Phi0*hT
           m = mod(n,8)
           if(m /= 0) then
              do i=1, m
                 f0         = freq(i+0)
                 c0         = twopi*f0*hT
                 sa0        = sin(c0)/c0
                 arg0       = 1.0_dp-(f0*hT*f0*hT)
                 Phi0f(i+0) = Phi0T*(sa0/arg0)
              end do
              if(n<8) return
           end if
           m1 = m+1
           !dir$ assume_aligned Phi0f:64
           !dir$ assume_aligned freq:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,8
              f0         = freq(i+0)
              c0         = twopi*f0*hT
              sa0        = sin(c0)/c0
              arg0       = 1.0_dp-(f0*hT*f0*hT)
              Phi0f(i+0) = Phi0T*(sa0/arg0)
              f1         = freq(i+1)
              c1         = twopi*f1*hT
              sa1        = sin(c1)/c1
              arg1       = 1.0_dp-(f1*hT*f1*hT)
              Phi0f(i+1) = Phi0T*(sa1/arg1)
              f2         = freq(i+2)
              c2         = twopi*f2*hT
              sa2        = sin(c2)/c2
              arg2       = 1.0_dp-(f2*hT*f2*hT)
              Phi0f(i+2) = Phi0T*(sa2/arg2)
              f3         = freq(i+3)
              c3         = twopi*f3*hT
              sa3        = sin(c3)/c3
              arg3       = 1.0_dp-(f3*hT*f3*hT)
              Phi0f(i+3) = Phi0T*(sa3/arg3)
              f4         = freq(i+4)
              c4         = twopi*f4*hT
              sa4        = sin(c4)/c4
              arg4       = 1.0_dp-(f4*hT*f4*hT)
              Phi0f(i+4) = Phi0T*(sa4/arg4)
              f5         = freq(i+5)
              c5         = twopi*f5*hT
              sa5        = sin(c5)/c5
              arg5       = 1.0_dp-(f5*hT*f5*hT)
              Phi0f(i+5) = Phi0T*(sa5/arg5)
              f6         = freq(i+6)
              c6         = twopi*f6*hT
              sa6        = sin(c6)/c6
              arg6       = 1.0_dp-(f6*hT*f6*hT)
              Phi0f(i+6) = Phi0T*(sa6/arg6)
              f7         = freq(i+7)
              c7         = twopi*f7*hT
              sa7        = sin(c7)/c7
              arg7       = 1.0_dp-(f7*hT*f7*hT)
              Phi0f(i+7) = Phi0T*(sa7/arg7)
          end do
          
       end subroutine const_flux_spectr_unroll_8x_r8


       subroutine const_flux_spectr_unroll_4x_r8(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_4x_r8
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_4x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_4x_r8
           real(kind=dp),  dimension(1:n), intent(out) :: Phi0f
           real(kind=dp),                  intent(in)  :: Phi0
           real(kind=dp),  dimension(1:n), intent(in)  :: freq
           integer(kind=i4),               intent(in)  :: n
           real(kind=dp),                  intent(in)  :: T
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp) :: arg0,arg1,arg2,arg3
           real(kind=dp) :: f0,f1,f2,f3
           real(kind=dp) :: c0,c1,c2,c3
           real(kind=dp) :: sa0,sa1,sa2,sa3
           real(kind=dp), automatic :: hT,Phi0T
           integer(kind=i4) :: i,m,m1
           hT    = 0.5_dp*T
           Phi0T = Phi0*hT
           m = mod(n,4)
           if(m /= 0) then
              do i=1, m
                 f0         = freq(i+0)
                 c0         = twopi*f0*hT
                 sa0        = sin(c0)/c0
                 arg0       = 1.0_dp-(f0*hT*f0*hT)
                 Phi0f(i+0) = Phi0T*(sa0/arg0)
              end do
              if(n<4) return
           end if
           m1 = m+1
           !dir$ assume_aligned Phi0f:64
           !dir$ assume_aligned freq:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,4
              f0         = freq(i+0)
              c0         = twopi*f0*hT
              sa0        = sin(c0)/c0
              arg0       = 1.0_dp-(f0*hT*f0*hT)
              Phi0f(i+0) = Phi0T*(sa0/arg0)
              f1         = freq(i+1)
              c1         = twopi*f1*hT
              sa1        = sin(c1)/c1
              arg1       = 1.0_dp-(f1*hT*f1*hT)
              Phi0f(i+1) = Phi0T*(sa1/arg1)
              f2         = freq(i+2)
              c2         = twopi*f2*hT
              sa2        = sin(c2)/c2
              arg2       = 1.0_dp-(f2*hT*f2*hT)
              Phi0f(i+2) = Phi0T*(sa2/arg2)
              f3         = freq(i+3)
              c3         = twopi*f3*hT
              sa3        = sin(c3)/c3
              arg3       = 1.0_dp-(f3*hT*f3*hT)
              Phi0f(i+3) = Phi0T*(sa3/arg3)
          end do
          
       end subroutine const_flux_spectr_unroll_4x_r8

       
       subroutine const_flux_spectr_unroll_2x_r8(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_2x_r8
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_2x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_2x_r8
           real(kind=dp),  dimension(1:n), intent(out) :: Phi0f
           real(kind=dp),                  intent(in)  :: Phi0
           real(kind=dp),  dimension(1:n), intent(in)  :: freq
           integer(kind=i4),               intent(in)  :: n
           real(kind=dp),                  intent(in)  :: T
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp) :: arg0,arg1
           real(kind=dp) :: f0,f1
           real(kind=dp) :: c0,c1
           real(kind=dp) :: sa0,sa1
           real(kind=dp), automatic :: hT,Phi0T
           integer(kind=i4) :: i,m,m1
           hT    = 0.5_dp*T
           Phi0T = Phi0*hT
           m = mod(n,2)
           if(m /= 0) then
              do i=1, m
                 f0         = freq(i+0)
                 c0         = twopi*f0*hT
                 sa0        = sin(c0)/c0
                 arg0       = 1.0_dp-(f0*hT*f0*hT)
                 Phi0f(i+0) = Phi0T*(sa0/arg0)
              end do
              if(n<2) return
           end if
           m1 = m+1
           !dir$ assume_aligned Phi0f:64
           !dir$ assume_aligned freq:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,2
              f0         = freq(i+0)
              c0         = twopi*f0*hT
              sa0        = sin(c0)/c0
              arg0       = 1.0_dp-(f0*hT*f0*hT)
              Phi0f(i+0) = Phi0T*(sa0/arg0)
              f1         = freq(i+1)
              c1         = twopi*f1*hT
              sa1        = sin(c1)/c1
              arg1       = 1.0_dp-(f1*hT*f1*hT)
              Phi0f(i+1) = Phi0T*(sa1/arg1)
            end do
          
       end subroutine const_flux_spectr_unroll_2x_r8

       

       subroutine const_flux_spectr_r8(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_r8
           !dir$ attributes forceinline ::   const_flux_spectr_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_r8
           real(kind=dp),  dimension(1:n), intent(out) :: Phi0f
           real(kind=dp),                  intent(in)  :: Phi0
           real(kind=dp),  dimension(1:n), intent(in)  :: freq
           integer(kind=i4),               intent(in)  :: n
           real(kind=dp),                  intent(in)  :: T
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp) :: arg0
           real(kind=dp) :: f0
           real(kind=dp) :: c0
           real(kind=dp) :: sa0
           real(kind=dp), automatic :: hT,Phi0T
           integer(kind=i4) :: i
           hT    = 0.5_dp*T
           Phi0T = Phi0*hT
         
           !dir$ assume_aligned Phi0f:64
           !dir$ assume_aligned freq:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=1,n
              f0         = freq(i+0)
              c0         = twopi*f0*hT
              sa0        = sin(c0)/c0
              arg0       = 1.0_dp-(f0*hT*f0*hT)
              Phi0f(i+0) = Phi0T*(sa0/arg0)
            
           end do
          
       end subroutine const_flux_spectr_unroll_r8

       
       subroutine const_flux_spectr_exec_r4(Phi0f,Phi0,freq,n,T,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_exec_r4
           real(kind=sp),  dimension(1:n), intent(out) :: Phi0f
           real(kind=sp),                  intent(in)  :: Phi0
           real(kind=sp),  dimension(1:n), intent(in)  :: freq
           integer(kind=i4),               intent(in)  :: n
           real(kind=sp),                  intent(in)  :: T
           integer(kind=i4),               intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                call const_flux_spectr_unroll_16x_r4(Phi0f,Phi0,freq,n,T)
              case (8)
                call const_flux_spectr_unroll_8x_r4(Phi0f,Phi0,freq,n,T)
              case (4)
                call const_flux_spectr_unroll_4x_r4(Phi0f,Phi0,freq,n,T)
              case (2)
                call const_flux_spectr_unroll_2x_r4(Phi0f,Phi0,freq,n,T) 
              case (0)
                call const_flux_spectr_r4(Phi0f,Phi0,freq,n,T)
              case default
                return
           end select
       end subroutine const_flux_spectr_exec_r4


       subroutine const_flux_spectr_exec_r8(Phi0f,Phi0,freq,n,T,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_exec_r8
           real(kind=dp),  dimension(1:n), intent(out) :: Phi0f
           real(kind=dp),                  intent(in)  :: Phi0
           real(kind=dp),  dimension(1:n), intent(in)  :: freq
           integer(kind=i4),               intent(in)  :: n
           real(kind=dp),                  intent(in)  :: T
           integer(kind=i4),               intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                call const_flux_spectr_unroll_16x_r8(Phi0f,Phi0,freq,n,T)
              case (8)
                call const_flux_spectr_unroll_8x_r8(Phi0f,Phi0,freq,n,T)
              case (4)
                call const_flux_spectr_unroll_4x_r8(Phi0f,Phi0,freq,n,T)
              case (2)
                call const_flux_spectr_unroll_2x_r8(Phi0f,Phi0,freq,n,T) 
              case (0)
                call const_flux_spectr_r8(Phi0f,Phi0,freq,n,T)
              case default
                return
           end select
       end subroutine const_flux_spectr_exec_r8
      
     
        !Идеальный гармонический модулятор
        !Formula 1,2 p. 187
        subroutine ideal_modulator_unroll_16x_r4(rhot_s,rhot_c,n,f0,phi0,rho0,rho1)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  ideal_modulator_unroll_16x_r4
           !dir$ attributes forceinline ::   ideal_modulator_unroll_16x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ideal_modulator_unroll_16x_r4
           real(kind=sp), dimension(1:n), intent(out) :: rhot_s
           real(kind=sp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp),                 intent(in)  :: f0
           real(kind=sp),                 intent(in)  :: phi0
           real(kind=sp),                 intent(in)  :: rho0
           real(kind=sp),                 intent(in)  :: rho1
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp) :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=sp) :: t8,t9,t10,t11,t12,t13,t14,t15
           real(kind=sp) :: s0,s1,s2,s3,s4,s5,s6,s7
           real(kind=sp) :: s8,s9,s10,s11,s12,s13,s14,s15
           real(kind=sp) :: c0,c1,c2,c3,c4,c5,c6,c7
           real(kind=sp) :: c8,c9,c10,c11,c12,c13,c14,c15
           real(kind=sp) :: psi0,psi1,psi2,psi3,psi4,psi5,psi6,psi7
           real(kind=sp) :: psi8,psi9,psi10,psi11,psi12,psi13,psi14,psi15
           real(kind=sp) :: om0
           integer(kind=i4) :: i,m,m1
           om0 = twopi*f0
           m=mod(n,16)
           if(m /= 0) then
              do i=1, m
                 t0        = real(i,kind=sp)
                 psi0      = om0*t0+phi0
                 s0        = rho0+rho1*sin(psi0)
                 rhot_s(i) = s0
                 c0        = rho0+rho1*cos(psi0)
                 rhot_c(i) = c0
              end do
              if(n<16) return
           end if
           m1 = m+1
           !dir$ assume_aligned rhot_s:64           
           !dir$ assume_aligned rhot_c:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,16
              t0          = real(i+0,kind=sp)
              psi0        = om0*t0+phi0
              s0          = rho0+rho1*sin(psi0)
              rhot_s(i+0) = s0
              c0          = rho0+rho1*cos(psi0)
              rhot_c(i+0) = c0
              t1          = real(i+1,kind=sp)
              psi1        = om0*t1+phi0
              s1          = rho0+rho1*sin(psi1)
              rhot_s(i+1) = s1
              c0          = rho0+rho1*cos(psi1)
              rhot_c(i+1) = c1
              t2          = real(i+2,kind=sp)
              psi2        = om0*t2+phi0
              s2          = rho0+rho1*sin(psi2)
              rhot_s(i+2) = s2
              c2          = rho0+rho1*cos(psi2)
              rhot_c(i+2) = c2
              t3          = real(i+3,kind=sp)
              psi3        = om0*t3+phi0
              s3          = rho0+rho1*sin(psi3)
              rhot_s(i+3) = s3
              c3          = rho0+rho1*cos(psi3)
              rhot_c(i+3) = c3
              t4          = real(i+4,kind=sp)
              psi4        = om0*t4+phi0
              s4          = rho0+rho1*sin(psi4)
              rhot_s(i+4) = s4
              c4          = rho0+rho1*cos(psi4)
              rhot_c(i+4) = c4
              t5          = real(i+5,kind=sp)
              psi5        = om0*t5+phi0
              s5          = rho0+rho1*sin(psi5)
              rhot_s(i+5) = s5
              c5          = rho0+rho1*cos(psi5)
              rhot_c(i+5) = c5
              t6          = real(i+6,kind=sp)
              psi6        = om0*t6+phi0
              s6          = rho0+rho1*sin(psi6)
              rhot_s(i+6) = s6
              c6          = rho0+rho1*cos(psi6)
              rhot_c(i+6) = c6
              t7          = real(i+7,kind=sp)
              psi7        = om0*t7+phi0
              s7          = rho0+rho1*sin(psi7)
              rhot_s(i+7) = s7
              c7          = rho0+rho1*cos(psi7)
              rhot_c(i+7) = c7
              t8          = real(i+8,kind=sp)
              psi8        = om0*t8+phi0
              s8          = rho0+rho1*sin(psi8)
              rhot_s(i+8) = s8
              c8          = rho0+rho1*cos(psi8)
              rhot_c(i+8) = c8
              t9          = real(i+9,kind=sp)
              psi9        = om0*t9+phi0
              s9          = rho0+rho1*sin(psi9)
              rhot_s(i+9) = s9
              c9          = rho0+rho1*cos(psi9)
              rhot_c(i+9) = c9
              t10         = real(i+10,kind=sp)
              psi10       = om0*t10+phi0
              s10         = rho0+rho1*sin(psi10)
              rhot_s(i+10)= s10
              c10         = rho0+rho1*cos(psi10)
              rhot_c(i+10)= c10
              t11         = real(i+11,kind=sp)
              psi11       = om0*t11+phi0
              s11         = rho0+rho1*sin(psi11)
              rhot_s(i+11)= s11
              c11         = rho0+rho1*cos(psi11)
              rhot_c(i+11)= c11
              t12         = real(i+12,kind=sp)
              psi12       = om0*t12+phi0
              s12         = rho0+rho1*sin(psi12)
              rhot_s(i+12)= s12
              c12          = rho0+rho1*cos(psi12)
              rhot_c(i+12)= c12
              t13         = real(i+13,kind=sp)
              psi13       = om0*t13+phi0
              s13         = rho0+rho1*sin(psi13)
              rhot_s(i+13)= s13
              c13         = rho0+rho1*cos(psi13)
              rhot_c(i+13)= c13
              t14         = real(i+14,kind=sp)
              psi14       = om0*t14+phi0
              s14         = rho0+rho1*sin(psi14)
              rhot_s(i+14)= s14
              c14         = rho0+rho1*cos(psi14)
              rhot_c(i+14)= c14
              t15         = real(i+15,kind=sp)
              psi15       = om0*t15+phi0
              s15         = rho0+rho1*sin(psi15)
              rhot_s(i+15)= s15
              c15         = rho0+rho1*cos(psi15)
              rhot_c(i+15)= c15
              
           end do
        
       end subroutine ideal_modulator_unroll_16x_r4


       subroutine ideal_modulator_unroll_8x_r4(rhot_s,rhot_c,n,f0,phi0,rho0,rho1)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  ideal_modulator_unroll_8x_r4
           !dir$ attributes forceinline ::   ideal_modulator_unroll_8x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ideal_modulator_unroll_8x_r4
           real(kind=sp), dimension(1:n), intent(out) :: rhot_s
           real(kind=sp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp),                 intent(in)  :: f0
           real(kind=sp),                 intent(in)  :: phi0
           real(kind=sp),                 intent(in)  :: rho0
           real(kind=sp),                 intent(in)  :: rho1
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp) :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=sp) :: s0,s1,s2,s3,s4,s5,s6,s7
           real(kind=sp) :: c0,c1,c2,c3,c4,c5,c6,c7
           real(kind=sp) :: psi0,psi1,psi2,psi3,psi4,psi5,psi6,psi7
           real(kind=sp) :: om0
           integer(kind=i4) :: i,m,m1
           om0 = twopi*f0
           m=mod(n,8)
           if(m /= 0) then
              do i=1, m
                 t0        = real(i,kind=sp)
                 psi0      = om0*t0+phi0
                 s0        = rho0+rho1*sin(psi0)
                 rhot_s(i) = s0
                 c0        = rho0+rho1*cos(psi0)
                 rhot_c(i) = c0
              end do
              if(n<8) return
           end if
           m1 = m+1
           !dir$ assume_aligned rhot_s:64           
           !dir$ assume_aligned rhot_c:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,8
              t0          = real(i+0,kind=sp)
              psi0        = om0*t0+phi0
              s0          = rho0+rho1*sin(psi0)
              rhot_s(i+0) = s0
              c0          = rho0+rho1*cos(psi0)
              rhot_c(i+0) = c0
              t1          = real(i+1,kind=sp)
              psi1        = om0*t1+phi0
              s1          = rho0+rho1*sin(psi1)
              rhot_s(i+1) = s1
              c0          = rho0+rho1*cos(psi1)
              rhot_c(i+1) = c1
              t2          = real(i+2,kind=sp)
              psi2        = om0*t2+phi0
              s2          = rho0+rho1*sin(psi2)
              rhot_s(i+2) = s2
              c2          = rho0+rho1*cos(psi2)
              rhot_c(i+2) = c2
              t3          = real(i+3,kind=sp)
              psi3        = om0*t3+phi0
              s3          = rho0+rho1*sin(psi3)
              rhot_s(i+3) = s3
              c3          = rho0+rho1*cos(psi3)
              rhot_c(i+3) = c3
              t4          = real(i+4,kind=sp)
              psi4        = om0*t4+phi0
              s4          = rho0+rho1*sin(psi4)
              rhot_s(i+4) = s4
              c4          = rho0+rho1*cos(psi4)
              rhot_c(i+4) = c4
              t5          = real(i+5,kind=sp)
              psi5        = om0*t5+phi0
              s5          = rho0+rho1*sin(psi5)
              rhot_s(i+5) = s5
              c5          = rho0+rho1*cos(psi5)
              rhot_c(i+5) = c5
              t6          = real(i+6,kind=sp)
              psi6        = om0*t6+phi0
              s6          = rho0+rho1*sin(psi6)
              rhot_s(i+6) = s6
              c6          = rho0+rho1*cos(psi6)
              rhot_c(i+6) = c6
              t7          = real(i+7,kind=sp)
              psi7        = om0*t7+phi0
              s7          = rho0+rho1*sin(psi7)
              rhot_s(i+7) = s7
              c7          = rho0+rho1*cos(psi7)
              rhot_c(i+7) = c7
           end do
        
       end subroutine ideal_modulator_unroll_8x_r4


       subroutine ideal_modulator_unroll_4x_r4(rhot_s,rhot_c,n,f0,phi0,rho0,rho1)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  ideal_modulator_unroll_4x_r4
           !dir$ attributes forceinline ::   ideal_modulator_unroll_4x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ideal_modulator_unroll_4x_r4
           real(kind=sp), dimension(1:n), intent(out) :: rhot_s
           real(kind=sp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp),                 intent(in)  :: f0
           real(kind=sp),                 intent(in)  :: phi0
           real(kind=sp),                 intent(in)  :: rho0
           real(kind=sp),                 intent(in)  :: rho1
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp) :: t0,t1,t2,t3
           real(kind=sp) :: s0,s1,s2,s3
           real(kind=sp) :: c0,c1,c2,c3
           real(kind=sp) :: psi0,psi1,psi2,psi3
           real(kind=sp) :: om0
           integer(kind=i4) :: i,m,m1
           om0 = twopi*f0
           m=mod(n,4)
           if(m /= 0) then
              do i=1, m
                 t0        = real(i,kind=sp)
                 psi0      = om0*t0+phi0
                 s0        = rho0+rho1*sin(psi0)
                 rhot_s(i) = s0
                 c0        = rho0+rho1*cos(psi0)
                 rhot_c(i) = c0
              end do
              if(n<4) return
           end if
           m1 = m+1
           !dir$ assume_aligned rhot_s:64           
           !dir$ assume_aligned rhot_c:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,4
              t0          = real(i+0,kind=sp)
              psi0        = om0*t0+phi0
              s0          = rho0+rho1*sin(psi0)
              rhot_s(i+0) = s0
              c0          = rho0+rho1*cos(psi0)
              rhot_c(i+0) = c0
              t1          = real(i+1,kind=sp)
              psi1        = om0*t1+phi0
              s1          = rho0+rho1*sin(psi1)
              rhot_s(i+1) = s1
              c0          = rho0+rho1*cos(psi1)
              rhot_c(i+1) = c1
              t2          = real(i+2,kind=sp)
              psi2        = om0*t2+phi0
              s2          = rho0+rho1*sin(psi2)
              rhot_s(i+2) = s2
              c2          = rho0+rho1*cos(psi2)
              rhot_c(i+2) = c2
              t3          = real(i+3,kind=sp)
              psi3        = om0*t3+phi0
              s3          = rho0+rho1*sin(psi3)
              rhot_s(i+3) = s3
              c3          = rho0+rho1*cos(psi3)
              rhot_c(i+3) = c3
            end do
        
       end subroutine ideal_modulator_unroll_4x_r4


       subroutine ideal_modulator_unroll_2x_r4(rhot_s,rhot_c,n,f0,phi0,rho0,rho1)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  ideal_modulator_unroll_2x_r4
           !dir$ attributes forceinline ::   ideal_modulator_unroll_2x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ideal_modulator_unroll_2x_r4
           real(kind=sp), dimension(1:n), intent(out) :: rhot_s
           real(kind=sp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp),                 intent(in)  :: f0
           real(kind=sp),                 intent(in)  :: phi0
           real(kind=sp),                 intent(in)  :: rho0
           real(kind=sp),                 intent(in)  :: rho1
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp) :: t0,t1
           real(kind=sp) :: s0,s1
           real(kind=sp) :: c0,c1
           real(kind=sp) :: psi0,psi1
           real(kind=sp) :: om0
           integer(kind=i4) :: i,m,m1
           om0 = twopi*f0
           m=mod(n,2)
           if(m /= 0) then
              do i=1, m
                 t0        = real(i,kind=sp)
                 psi0      = om0*t0+phi0
                 s0        = rho0+rho1*sin(psi0)
                 rhot_s(i) = s0
                 c0        = rho0+rho1*cos(psi0)
                 rhot_c(i) = c0
              end do
              if(n<2) return
           end if
           m1 = m+1
           !dir$ assume_aligned rhot_s:64           
           !dir$ assume_aligned rhot_c:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,2
              t0          = real(i+0,kind=sp)
              psi0        = om0*t0+phi0
              s0          = rho0+rho1*sin(psi0)
              rhot_s(i+0) = s0
              c0          = rho0+rho1*cos(psi0)
              rhot_c(i+0) = c0
              t1          = real(i+1,kind=sp)
              psi1        = om0*t1+phi0
              s1          = rho0+rho1*sin(psi1)
              rhot_s(i+1) = s1
              c0          = rho0+rho1*cos(psi1)
              rhot_c(i+1) = c1
           end do
        
       end subroutine ideal_modulator_unroll_2x_r4


       subroutine ideal_modulator_r4(rhot_s,rhot_c,n,f0,phi0,rho0,rho1)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  ideal_modulator_r4
           !dir$ attributes forceinline ::   ideal_modulator_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ideal_modulator_r4
           real(kind=sp), dimension(1:n), intent(out) :: rhot_s
           real(kind=sp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp),                 intent(in)  :: f0
           real(kind=sp),                 intent(in)  :: phi0
           real(kind=sp),                 intent(in)  :: rho0
           real(kind=sp),                 intent(in)  :: rho1
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp) :: t0
           real(kind=sp) :: s0
           real(kind=sp) :: c0
           real(kind=sp) :: psi0
           real(kind=sp) :: om0
           integer(kind=i4) :: i
           om0 = twopi*f0
        
           !dir$ assume_aligned rhot_s:64           
           !dir$ assume_aligned rhot_c:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=1,n
              t0          = real(i+0,kind=sp)
              psi0        = om0*t0+phi0
              s0          = rho0+rho1*sin(psi0)
              rhot_s(i+0) = s0
              c0          = rho0+rho1*cos(psi0)
              rhot_c(i+0) = c0
           end do
        
       end subroutine ideal_modulator_r4



       subroutine ideal_modulator_unroll_16x_r8(rhot_s,rhot_c,n,f0,phi0,rho0,rho1)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  ideal_modulator_unroll_16x_r8
           !dir$ attributes forceinline ::   ideal_modulator_unroll_16x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ideal_modulator_unroll_16x_r8
           real(kind=dp), dimension(1:n), intent(out) :: rhot_s
           real(kind=dp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp),                 intent(in)  :: f0
           real(kind=dp),                 intent(in)  :: phi0
           real(kind=dp),                 intent(in)  :: rho0
           real(kind=dp),                 intent(in)  :: rho1
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp) :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=dp) :: t8,t9,t10,t11,t12,t13,t14,t15
           real(kind=dp) :: s0,s1,s2,s3,s4,s5,s6,s7
           real(kind=dp) :: s8,s9,s10,s11,s12,s13,s14,s15
           real(kind=dp) :: c0,c1,c2,c3,c4,c5,c6,c7
           real(kind=dp) :: c8,c9,c10,c11,c12,c13,c14,c15
           real(kind=dp) :: psi0,psi1,psi2,psi3,psi4,psi5,psi6,psi7
           real(kind=dp) :: psi8,psi9,psi10,psi11,psi12,psi13,psi14,psi15
           real(kind=dp) :: om0
           integer(kind=i4) :: i,m,m1
           om0 = twopi*f0
           m=mod(n,16)
           if(m /= 0) then
              do i=1, m
                 t0        = real(i,kind=sp)
                 psi0      = om0*t0+phi0
                 s0        = rho0+rho1*sin(psi0)
                 rhot_s(i) = s0
                 c0        = rho0+rho1*cos(psi0)
                 rhot_c(i) = c0
              end do
              if(n<16) return
           end if
           m1 = m+1
           !dir$ assume_aligned rhot_s:64           
           !dir$ assume_aligned rhot_c:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,16
              t0          = real(i+0,kind=dp)
              psi0        = om0*t0+phi0
              s0          = rho0+rho1*sin(psi0)
              rhot_s(i+0) = s0
              c0          = rho0+rho1*cos(psi0)
              rhot_c(i+0) = c0
              t1          = real(i+1,kind=dp)
              psi1        = om0*t1+phi0
              s1          = rho0+rho1*sin(psi1)
              rhot_s(i+1) = s1
              c0          = rho0+rho1*cos(psi1)
              rhot_c(i+1) = c1
              t2          = real(i+2,kind=dp)
              psi2        = om0*t2+phi0
              s2          = rho0+rho1*sin(psi2)
              rhot_s(i+2) = s2
              c2          = rho0+rho1*cos(psi2)
              rhot_c(i+2) = c2
              t3          = real(i+3,kind=dp)
              psi3        = om0*t3+phi0
              s3          = rho0+rho1*sin(psi3)
              rhot_s(i+3) = s3
              c3          = rho0+rho1*cos(psi3)
              rhot_c(i+3) = c3
              t4          = real(i+4,kind=dp)
              psi4        = om0*t4+phi0
              s4          = rho0+rho1*sin(psi4)
              rhot_s(i+4) = s4
              c4          = rho0+rho1*cos(psi4)
              rhot_c(i+4) = c4
              t5          = real(i+5,kind=dp)
              psi5        = om0*t5+phi0
              s5          = rho0+rho1*sin(psi5)
              rhot_s(i+5) = s5
              c5          = rho0+rho1*cos(psi5)
              rhot_c(i+5) = c5
              t6          = real(i+6,kind=dp)
              psi6        = om0*t6+phi0
              s6          = rho0+rho1*sin(psi6)
              rhot_s(i+6) = s6
              c6          = rho0+rho1*cos(psi6)
              rhot_c(i+6) = c6
              t7          = real(i+7,kind=dp)
              psi7        = om0*t7+phi0
              s7          = rho0+rho1*sin(psi7)
              rhot_s(i+7) = s7
              c7          = rho0+rho1*cos(psi7)
              rhot_c(i+7) = c7
              t8          = real(i+8,kind=dp)
              psi8        = om0*t8+phi0
              s8          = rho0+rho1*sin(psi8)
              rhot_s(i+8) = s8
              c8          = rho0+rho1*cos(psi8)
              rhot_c(i+8) = c8
              t9          = real(i+9,kind=dp)
              psi9        = om0*t9+phi0
              s9          = rho0+rho1*sin(psi9)
              rhot_s(i+9) = s9
              c9          = rho0+rho1*cos(psi9)
              rhot_c(i+9) = c9
              t10         = real(i+10,kind=dp)
              psi10       = om0*t10+phi0
              s10         = rho0+rho1*sin(psi10)
              rhot_s(i+10)= s10
              c10         = rho0+rho1*cos(psi10)
              rhot_c(i+10)= c10
              t11         = real(i+11,kind=dp)
              psi11       = om0*t11+phi0
              s11         = rho0+rho1*sin(psi11)
              rhot_s(i+11)= s11
              c11         = rho0+rho1*cos(psi11)
              rhot_c(i+11)= c11
              t12         = real(i+12,kind=dp)
              psi12       = om0*t12+phi0
              s12         = rho0+rho1*sin(psi12)
              rhot_s(i+12)= s12
              c4          = rho0+rho1*cos(psi12)
              rhot_c(i+12)= c12
              t13         = real(i+13,kind=dp)
              psi13       = om0*t13+phi0
              s13         = rho0+rho1*sin(psi13)
              rhot_s(i+13)= s13
              c13         = rho0+rho1*cos(psi13)
              rhot_c(i+13)= c13
              t14         = real(i+14,kind=dp)
              psi14       = om0*t14+phi0
              s14         = rho0+rho1*sin(psi14)
              rhot_s(i+14)= s14
              c14         = rho0+rho1*cos(psi14)
              rhot_c(i+14)= c14
              t15         = real(i+15,kind=dp)
              psi15       = om0*t15+phi0
              s15         = rho0+rho1*sin(psi15)
              rhot_s(i+15)= s15
              c15         = rho0+rho1*cos(psi15)
              rhot_c(i+15)= c15
              
           end do
        
       end subroutine ideal_modulator_unroll_16x_r8


       subroutine ideal_modulator_unroll_8x_r8(rhot_s,rhot_c,n,f0,phi0,rho0,rho1)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  ideal_modulator_unroll_8x_r8
           !dir$ attributes forceinline ::   ideal_modulator_unroll_8x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ideal_modulator_unroll_8x_r8
           real(kind=dp), dimension(1:n), intent(out) :: rhot_s
           real(kind=dp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp),                 intent(in)  :: f0
           real(kind=dp),                 intent(in)  :: phi0
           real(kind=dp),                 intent(in)  :: rho0
           real(kind=dp),                 intent(in)  :: rho1
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp) :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=dp) :: s0,s1,s2,s3,s4,s5,s6,s7
           real(kind=dp) :: c0,c1,c2,c3,c4,c5,c6,c7
           real(kind=dp) :: psi0,psi1,psi2,psi3,psi4,psi5,psi6,psi7
           real(kind=dp) :: om0
           integer(kind=i4) :: i,m,m1
           om0 = twopi*f0
           m=mod(n,8)
           if(m /= 0) then
              do i=1, m
                 t0        = real(i,kind=dp)
                 psi0      = om0*t0+phi0
                 s0        = rho0+rho1*sin(psi0)
                 rhot_s(i) = s0
                 c0        = rho0+rho1*cos(psi0)
                 rhot_c(i) = c0
              end do
              if(n<8) return
           end if
           m1 = m+1
           !dir$ assume_aligned rhot_s:64           
           !dir$ assume_aligned rhot_c:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,8
              t0          = real(i+0,kind=dp)
              psi0        = om0*t0+phi0
              s0          = rho0+rho1*sin(psi0)
              rhot_s(i+0) = s0
              c0          = rho0+rho1*cos(psi0)
              rhot_c(i+0) = c0
              t1          = real(i+1,kind=dp)
              psi1        = om0*t1+phi0
              s1          = rho0+rho1*sin(psi1)
              rhot_s(i+1) = s1
              c0          = rho0+rho1*cos(psi1)
              rhot_c(i+1) = c1
              t2          = real(i+2,kind=dp)
              psi2        = om0*t2+phi0
              s2          = rho0+rho1*sin(psi2)
              rhot_s(i+2) = s2
              c2          = rho0+rho1*cos(psi2)
              rhot_c(i+2) = c2
              t3          = real(i+3,kind=dp)
              psi3        = om0*t3+phi0
              s3          = rho0+rho1*sin(psi3)
              rhot_s(i+3) = s3
              c3          = rho0+rho1*cos(psi3)
              rhot_c(i+3) = c3
              t4          = real(i+4,kind=dp)
              psi4        = om0*t4+phi0
              s4          = rho0+rho1*sin(psi4)
              rhot_s(i+4) = s4
              c4          = rho0+rho1*cos(psi4)
              rhot_c(i+4) = c4
              t5          = real(i+5,kind=dp)
              psi5        = om0*t5+phi0
              s5          = rho0+rho1*sin(psi5)
              rhot_s(i+5) = s5
              c5          = rho0+rho1*cos(psi5)
              rhot_c(i+5) = c5
              t6          = real(i+6,kind=dp)
              psi6        = om0*t6+phi0
              s6          = rho0+rho1*sin(psi6)
              rhot_s(i+6) = s6
              c6          = rho0+rho1*cos(psi6)
              rhot_c(i+6) = c6
              t7          = real(i+7,kind=dp)
              psi7        = om0*t7+phi0
              s7          = rho0+rho1*sin(psi7)
              rhot_s(i+7) = s7
              c7          = rho0+rho1*cos(psi7)
              rhot_c(i+7) = c7
           end do
        
       end subroutine ideal_modulator_unroll_8x_r8


       subroutine ideal_modulator_unroll_4x_r8(rhot_s,rhot_c,n,f0,phi0,rho0,rho1)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  ideal_modulator_unroll_4x_r8
           !dir$ attributes forceinline ::   ideal_modulator_unroll_4x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ideal_modulator_unroll_4x_r8
           real(kind=dp), dimension(1:n), intent(out) :: rhot_s
           real(kind=dp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp),                 intent(in)  :: f0
           real(kind=dp),                 intent(in)  :: phi0
           real(kind=dp),                 intent(in)  :: rho0
           real(kind=dp),                 intent(in)  :: rho1
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp) :: t0,t1,t2,t3
           real(kind=dp) :: s0,s1,s2,s3
           real(kind=dp) :: c0,c1,c2,c3
           real(kind=dp) :: psi0,psi1,psi2,psi3
           real(kind=dp) :: om0
           integer(kind=i4) :: i,m,m1
           om0 = twopi*f0
           m=mod(n,4)
           if(m /= 0) then
              do i=1, m
                 t0        = real(i,kind=dp)
                 psi0      = om0*t0+phi0
                 s0        = rho0+rho1*sin(psi0)
                 rhot_s(i) = s0
                 c0        = rho0+rho1*cos(psi0)
                 rhot_c(i) = c0
              end do
              if(n<4) return
           end if
           m1 = m+1
           !dir$ assume_aligned rhot_s:64           
           !dir$ assume_aligned rhot_c:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,4
              t0          = real(i+0,kind=dp)
              psi0        = om0*t0+phi0
              s0          = rho0+rho1*sin(psi0)
              rhot_s(i+0) = s0
              c0          = rho0+rho1*cos(psi0)
              rhot_c(i+0) = c0
              t1          = real(i+1,kind=dp)
              psi1        = om0*t1+phi0
              s1          = rho0+rho1*sin(psi1)
              rhot_s(i+1) = s1
              c0          = rho0+rho1*cos(psi1)
              rhot_c(i+1) = c1
              t2          = real(i+2,kind=dp)
              psi2        = om0*t2+phi0
              s2          = rho0+rho1*sin(psi2)
              rhot_s(i+2) = s2
              c2          = rho0+rho1*cos(psi2)
              rhot_c(i+2) = c2
              t3          = real(i+3,kind=dp)
              psi3        = om0*t3+phi0
              s3          = rho0+rho1*sin(psi3)
              rhot_s(i+3) = s3
              c3          = rho0+rho1*cos(psi3)
              rhot_c(i+3) = c3
            end do
        
       end subroutine ideal_modulator_unroll_4x_r8


       subroutine ideal_modulator_unroll_2x_r8(rhot_s,rhot_c,n,f0,phi0,rho0,rho1)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  ideal_modulator_unroll_2x_r8
           !dir$ attributes forceinline ::   ideal_modulator_unroll_2x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ideal_modulator_unroll_2x_r8
           real(kind=dp), dimension(1:n), intent(out) :: rhot_s
           real(kind=dp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp),                 intent(in)  :: f0
           real(kind=dp),                 intent(in)  :: phi0
           real(kind=dp),                 intent(in)  :: rho0
           real(kind=dp),                 intent(in)  :: rho1
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp) :: t0,t1
           real(kind=dp) :: s0,s1
           real(kind=dp) :: c0,c1
           real(kind=dp) :: psi0,psi1
           real(kind=dp) :: om0
           integer(kind=i4) :: i,m,m1
           om0 = twopi*f0
           m=mod(n,2)
           if(m /= 0) then
              do i=1, m
                 t0        = real(i,kind=dp)
                 psi0      = om0*t0+phi0
                 s0        = rho0+rho1*sin(psi0)
                 rhot_s(i) = s0
                 c0        = rho0+rho1*cos(psi0)
                 rhot_c(i) = c0
              end do
              if(n<2) return
           end if
           m1 = m+1
           !dir$ assume_aligned rhot_s:64           
           !dir$ assume_aligned rhot_c:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,2
              t0          = real(i+0,kind=dp)
              psi0        = om0*t0+phi0
              s0          = rho0+rho1*sin(psi0)
              rhot_s(i+0) = s0
              c0          = rho0+rho1*cos(psi0)
              rhot_c(i+0) = c0
              t1          = real(i+1,kind=dp)
              psi1        = om0*t1+phi0
              s1          = rho0+rho1*sin(psi1)
              rhot_s(i+1) = s1
              c0          = rho0+rho1*cos(psi1)
              rhot_c(i+1) = c1
           end do
        
       end subroutine ideal_modulator_unroll_2x_r8


       subroutine ideal_modulator_r8(rhot_s,rhot_c,n,f0,phi0,rho0,rho1)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  ideal_modulator_r8
           !dir$ attributes forceinline ::   ideal_modulator_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ideal_modulator_r8
           real(kind=dp), dimension(1:n), intent(out) :: rhot_s
           real(kind=dp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp),                 intent(in)  :: f0
           real(kind=dp),                 intent(in)  :: phi0
           real(kind=dp),                 intent(in)  :: rho0
           real(kind=dp),                 intent(in)  :: rho1
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp) :: t0
           real(kind=dp) :: s0
           real(kind=dp) :: c0
           real(kind=dp) :: psi0
           real(kind=dp) :: om0
           integer(kind=i4) :: i
           om0 = twopi*f0
         
           !dir$ assume_aligned rhot_s:64           
           !dir$ assume_aligned rhot_c:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=1,n
              t0          = real(i+0,kind=dp)
              psi0        = om0*t0+phi0
              s0          = rho0+rho1*sin(psi0)
              rhot_s(i+0) = s0
              c0          = rho0+rho1*cos(psi0)
              rhot_c(i+0) = c0
           end do
        
       end subroutine ideal_modulator_r8


       subroutine ideal_modulator_exec_r4(rhot_s,rhot_c,n,f0,phi0,rho0,rho1,unroll_cnt)
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: ideal_modulator_exec_r4
           real(kind=sp), dimension(1:n), intent(out) :: rhot_s
           real(kind=sp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp),                 intent(in)  :: f0
           real(kind=sp),                 intent(in)  :: phi0
           real(kind=sp),                 intent(in)  :: rho0
           real(kind=sp),                 intent(in)  :: rho1
           integer(kind=i4),              intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                call ideal_modulator_unroll_16x_r4(rhot_s,rhot_c,n,f0,phi0,rho0,rho1)
              case (8)
                call ideal_modulator_unroll_8x_r4(rhot_s,rhot_c,n,f0,phi0,rho0,rho1)
              case (4)
                call ideal_modulator_unroll_4x_r4(rhot_s,rhot_c,n,f0,phi0,rho0,rho1)
              case (2)
                call ideal_modulator_unroll_2x_r4(rhot_s,rhot_c,n,f0,phi0,rho0,rho1) 
              case (0)
                call ideal_modulator_r4(rhot_s,rhot_c,n,f0,phi0,rho0,rho1)
              case default
                return
            end select
       end subroutine ideal_modulator_exec_r4


       subroutine ideal_modulator_exec_r8(rhot_s,rhot_c,n,f0,phi0,rho0,rho1,unroll_cnt)
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: ideal_modulator_exec_r8
           real(kind=dp), dimension(1:n), intent(out) :: rhot_s
           real(kind=dp), dimension(1:n), intent(out) :: rhot_c
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp),                 intent(in)  :: f0
           real(kind=dp),                 intent(in)  :: phi0
           real(kind=dp),                 intent(in)  :: rho0
           real(kind=dp),                 intent(in)  :: rho1
           integer(kind=i4),              intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                call ideal_modulator_unroll_16x_r8(rhot_s,rhot_c,n,f0,phi0,rho0,rho1)
              case (8)
                call ideal_modulator_unroll_8x_r8(rhot_s,rhot_c,n,f0,phi0,rho0,rho1)
              case (4)
                call ideal_modulator_unroll_4x_r8(rhot_s,rhot_c,n,f0,phi0,rho0,rho1)
              case (2)
                call ideal_modulator_unroll_2x_r8(rhot_s,rhot_c,n,f0,phi0,rho0,rho1) 
              case (0)
                call ideal_modulator_r8(rhot_s,rhot_c,n,f0,phi0,rho0,rho1)
              case default
                return
            end select
       end subroutine ideal_modulator_exec_r8




       

! To be correctly reimplemented.
       

    

        !Ошибни изготовления растра —
        !модулятора излучения
        !Formula 2, p. 189
        subroutine rect_pulse_flux_unroll_16x_r4(Phik,fk,Phi0,n,Tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_flux_unroll_16x_r4
           !dir$ attributes forceinline ::   rect_pulse_flux_unroll_16x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_flux_unroll_16x_r4
           real(kind=sp), dimension(1:n), intent(out) :: Phik
           real(kind=sp), dimension(1:n), intent(in)  :: fk
           real(kind=sp),                 intent(in)  :: Phi0
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp),                 intent(in)  :: Tin
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp) :: fk0,fk1,fk2,fk3,fk4,fk5,fk6,fk7
           real(kind=sp) :: fk8,fk9,fk10,fk11,fk12,fk13,fk14,fk15
           real(kind=sp) :: sinc0,sinc1,sinc2,sinc3,sinc4,sinc5,sinc6,sinc7
           real(kind=sp) :: sinc8,sinc9,sinc10,sinc11,sinc12,sinc13,sinc14,sinc15
           real(kind=sp) :: arg0,arg1,arg2,arg3,arg4,arg,arg6,arg7
           real(kind=sp) :: arg1,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           real(kind=sp) :: hTin,Phi0fk
           integer(kind=i4) :: i,m,m1
           hTin   = 0.5_sp*Tin
           Phi0fk = Phi0*Tin 
           m=mod(n,16)
           if(m /= 0) then
              do i=1, m
                 fk0       = fk(i+0)
                 arg0      = twopi*fk0*hTin
                 sinc0     = sin(arg0)/arg0
                 Phik(i+0) = Phi0fk*sinc0
              end do
              if(n<16) return
           end if
           m1 = m+1
           !dir$ assume_aligned Phik:64
           !dir$ assume_aligned fk:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,16
                fk0       = fk(i+0)
                arg0      = twopi*fk0*hTin
                sinc0     = sin(arg0)/arg0
                Phik(i+0) = Phi0fk*sinc0
                fk1       = fk(i+1)
                arg1      = twopi*fk1*hTin
                sinc1     = sin(arg1)/arg1
                Phik(i+1) = Phi0fk*sinc1
                fk2       = fk(i+2)
                arg2      = twopi*fk2*hTin
                sinc2     = sin(arg2)/arg2
                Phik(i+2) = Phi0fk*sinc2
                fk3       = fk(i+3)
                arg3      = twopi*fk3*hTin
                sinc3     = sin(arg3)/arg3
                Phik(i+3) = Phi0fk*sinc3
                fk4       = fk(i+4)
                arg4      = twopi*fk4*hTin
                sinc4     = sin(arg4)/arg4
                Phik(i+4) = Phi0fk*sinc4
                fk5       = fk(i+5)
                arg5      = twopi*fk5*hTin
                sinc5     = sin(arg5)/arg5
                Phik(i+5) = Phi0fk*sinc5
                fk6       = fk(i+6)
                arg6      = twopi*fk6*hTin
                sinc6     = sin(arg6)/arg6
                Phik(i+6) = Phi0fk*sinc6
                fk7       = fk(i+7)
                arg7      = twopi*fk7*hTin
                sinc7     = sin(arg7)/arg7
                Phik(i+7) = Phi0fk*sinc7
                fk8       = fk(i+8)
                arg8      = twopi*fk8*hTin
                sinc8     = sin(arg8)/arg8
                Phik(i+8) = Phi0fk*sinc8
                fk9       = fk(i+9)
                arg9      = twopi*fk9*hTin
                sinc9     = sin(arg9)/arg9
                Phik(i+9) = Phi0fk*sinc1
                fk10      = fk(i+10)
                arg10     = twopi*fk10*hTin
                sinc10    = sin(arg10)/arg10
                Phik(i+10)= Phi0fk*sinc10
                fk11      = fk(i+11)
                arg11     = twopi*fk11*hTin
                sinc11    = sin(arg11)/arg11
                Phik(i+11)= Phi0fk*sinc3
                fk12      = fk(i+12)
                arg12     = twopi*fk12*hTin
                sinc12    = sin(arg12)/arg12
                Phik(i+12)= Phi0fk*sinc12
                fk13      = fk(i+13)
                arg13     = twopi*fk13*hTin
                sinc13    = sin(arg13)/arg13
                Phik(i+13)= Phi0fk*sinc13
                fk14      = fk(i+14)
                arg14     = twopi*fk14*hTin
                sinc14    = sin(arg14)/arg14
                Phik(i+14)= Phi0fk*sinc14
                fk15      = fk(i+15)
                arg15     = twopi*fk15*hTin
                sinc15    = sin(arg15)/arg15
                Phik(i+15)= Phi0fk*sinc15
           end do
          
        end subroutine rect_pulse_flux_unroll_16x_r4


        subroutine rect_pulse_flux_unroll_8x_r4(Phik,fk,Phi0,n,Tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_flux_unroll_8x_r4
           !dir$ attributes forceinline ::   rect_pulse_flux_unroll_8x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_flux_unroll_8x_r4
           real(kind=sp), dimension(1:n), intent(out) :: Phik
           real(kind=sp), dimension(1:n), intent(in)  :: fk
           real(kind=sp),                 intent(in)  :: Phi0
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp),                 intent(in)  :: Tin
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp) :: fk0,fk1,fk2,fk3,fk4,fk5,fk6,fk7
           real(kind=sp) :: sinc0,sinc1,sinc2,sinc3,sinc4,sinc5,sinc6,sinc7
           real(kind=sp) :: arg0,arg1,arg2,arg3,arg4,arg,arg6,arg7
           real(kind=sp) :: hTin,Phi0fk
           integer(kind=i4) :: i,m,m1
           hTin   = 0.5_sp*Tin
           Phi0fk = Phi0*Tin 
           m=mod(n,8)
           if(m /= 0) then
              do i=1, m
                 fk0       = fk(i)
                 arg0      = twopi*fk0*hTin
                 sinc0     = sin(arg0)/arg0
                 Phik(i) = Phi0fk*sinc0
              end do
              if(n<8) return
           end if
           m1 = m+1
           !dir$ assume_aligned Phik:64
           !dir$ assume_aligned fk:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,8
                fk0       = fk(i+0)
                arg0      = twopi*fk0*hTin
                sinc0     = sin(arg0)/arg0
                Phik(i+0) = Phi0fk*sinc0
                fk1       = fk(i+1)
                arg1      = twopi*fk1*hTin
                sinc1     = sin(arg1)/arg1
                Phik(i+1) = Phi0fk*sinc1
                fk2       = fk(i+2)
                arg2      = twopi*fk2*hTin
                sinc2     = sin(arg2)/arg2
                Phik(i+2) = Phi0fk*sinc2
                fk3       = fk(i+3)
                arg3      = twopi*fk3*hTin
                sinc3     = sin(arg3)/arg3
                Phik(i+3) = Phi0fk*sinc3
                fk4       = fk(i+4)
                arg4      = twopi*fk4*hTin
                sinc4     = sin(arg4)/arg4
                Phik(i+4) = Phi0fk*sinc4
                fk5       = fk(i+5)
                arg5      = twopi*fk5*hTin
                sinc5     = sin(arg5)/arg5
                Phik(i+5) = Phi0fk*sinc5
                fk6       = fk(i+6)
                arg6      = twopi*fk6*hTin
                sinc6     = sin(arg6)/arg6
                Phik(i+6) = Phi0fk*sinc6
                fk7       = fk(i+7)
                arg7      = twopi*fk7*hTin
                sinc7     = sin(arg7)/arg7
                Phik(i+7) = Phi0fk*sinc7
             end do
          
        end subroutine rect_pulse_flux_unroll_8x_r4


        subroutine rect_pulse_flux_unroll_4x_r4(Phik,fk,Phi0,n,Tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_flux_unroll_4x_r4
           !dir$ attributes forceinline ::   rect_pulse_flux_unroll_4x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_flux_unroll_4x_r4
           real(kind=sp), dimension(1:n), intent(out) :: Phik
           real(kind=sp), dimension(1:n), intent(in)  :: fk
           real(kind=sp),                 intent(in)  :: Phi0
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp),                 intent(in)  :: Tin
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp) :: fk0,fk1,fk2,fk3
           real(kind=sp) :: sinc0,sinc1,sinc2,sinc3
           real(kind=sp) :: arg0,arg1,arg2,arg3
           real(kind=sp) :: hTin,Phi0fk
           integer(kind=i4) :: i,m,m1
           hTin   = 0.5_sp*Tin
           Phi0fk = Phi0*Tin 
           m=mod(n,4)
           if(m /= 0) then
              do i=1, m
                 fk0       = fk(i)
                 arg0      = twopi*fk0*hTin
                 sinc0     = sin(arg0)/arg0
                 Phik(i) = Phi0fk*sinc0
              end do
              if(n<4) return
           end if
           m1 = m+1
           !dir$ assume_aligned Phik:64
           !dir$ assume_aligned fk:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,4
                fk0       = fk(i+0)
                arg0      = twopi*fk0*hTin
                sinc0     = sin(arg0)/arg0
                Phik(i+0) = Phi0fk*sinc0
                fk1       = fk(i+1)
                arg1      = twopi*fk1*hTin
                sinc1     = sin(arg1)/arg1
                Phik(i+1) = Phi0fk*sinc1
                fk2       = fk(i+2)
                arg2      = twopi*fk2*hTin
                sinc2     = sin(arg2)/arg2
                Phik(i+2) = Phi0fk*sinc2
                fk3       = fk(i+3)
                arg3      = twopi*fk3*hTin
                sinc3     = sin(arg3)/arg3
                Phik(i+3) = Phi0fk*sinc3
             end do
          
        end subroutine rect_pulse_flux_unroll_4x_r4


        subroutine rect_pulse_flux_unroll_2x_r4(Phik,fk,Phi0,n,Tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_flux_unroll_2x_r4
           !dir$ attributes forceinline ::   rect_pulse_flux_unroll_2x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_flux_unroll_2x_r4
           real(kind=sp), dimension(1:n), intent(out) :: Phik
           real(kind=sp), dimension(1:n), intent(in)  :: fk
           real(kind=sp),                 intent(in)  :: Phi0
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp),                 intent(in)  :: Tin
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp) :: fk0,fk1
           real(kind=sp) :: sinc0,sinc1
           real(kind=sp) :: arg0,arg1
           real(kind=sp) :: hTin,Phi0fk
           integer(kind=i4) :: i,m,m1
           hTin   = 0.5_sp*Tin
           Phi0fk = Phi0*Tin 
           m=mod(n,2)
           if(m /= 0) then
              do i=1, m
                 fk0       = fk(i)
                 arg0      = twopi*fk0*hTin
                 sinc0     = sin(arg0)/arg0
                 Phik(i) = Phi0fk*sinc0
              end do
              if(n<2) return
           end if
           m1 = m+1
           !dir$ assume_aligned Phik:64
           !dir$ assume_aligned fk:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,2
                fk0       = fk(i+0)
                arg0      = twopi*fk0*hTin
                sinc0     = sin(arg0)/arg0
                Phik(i+0) = Phi0fk*sinc0
                fk1       = fk(i+1)
                arg1      = twopi*fk1*hTin
                sinc1     = sin(arg1)/arg1
                Phik(i+1) = Phi0fk*sinc1
     
             end do
          
        end subroutine rect_pulse_flux_unroll_2x_r4


        subroutine rect_pulse_flux_r4(Phik,fk,Phi0,n,Tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_flux_r4
           !dir$ attributes forceinline ::   rect_pulse_flux_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_flux_r4
           real(kind=sp), dimension(1:n), intent(out) :: Phik
           real(kind=sp), dimension(1:n), intent(in)  :: fk
           real(kind=sp),                 intent(in)  :: Phi0
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp),                 intent(in)  :: Tin
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp) :: fk0
           real(kind=sp) :: sinc0
           real(kind=sp) :: arg0
           real(kind=sp) :: hTin,Phi0fk
           integer(kind=i4) :: i
           hTin   = 0.5_sp*Tin
           Phi0fk = Phi0*Tin 
          
           !dir$ assume_aligned Phik:64
           !dir$ assume_aligned fk:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=1,n
                fk0       = fk(i+0)
                arg0      = twopi*fk0*hTin
                sinc0     = sin(arg0)/arg0
                Phik(i+0) = Phi0fk*sinc0
             
           end do
          
        end subroutine rect_pulse_flux_r4




        
       subroutine rect_pulse_flux_unroll_16x_r8(Phik,fk,Phi0,n,Tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_flux_unroll_16x_r8
           !dir$ attributes forceinline ::   rect_pulse_flux_unroll_16x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_flux_unroll_16x_r8
           real(kind=dp), dimension(1:n), intent(out) :: Phik
           real(kind=dp), dimension(1:n), intent(in)  :: fk
           real(kind=dp),                 intent(in)  :: Phi0
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp),                 intent(in)  :: Tin
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp) :: fk0,fk1,fk2,fk3,fk4,fk5,fk6,fk7
           real(kind=dp) :: fk8,fk9,fk10,fk11,fk12,fk13,fk14,fk15
           real(kind=dp) :: sinc0,sinc1,sinc2,sinc3,sinc4,sinc5,sinc6,sinc7
           real(kind=dp) :: sinc8,sinc9,sinc10,sinc11,sinc12,sinc13,sinc14,sinc15
           real(kind=dp) :: arg0,arg1,arg2,arg3,arg4,arg,arg6,arg7
           real(kind=dp) :: arg1,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           real(kind=dp) :: hTin,Phi0fk
           integer(kind=i4) :: i,m,m1
           hTin   = 0.5_dp*Tin
           Phi0fk = Phi0*Tin 
           m=mod(n,16)
           if(m /= 0) then
              do i=1, m
                 fk0       = fk(i)
                 arg0      = twopi*fk0*hTin
                 sinc0     = sin(arg0)/arg0
                 Phik(i)   = Phi0fk*sinc0
              end do
              if(n<16) return
           end if
           m1 = m+1
           !dir$ assume_aligned Phik:64
           !dir$ assume_aligned fk:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,16
                fk0       = fk(i+0)
                arg0      = twopi*fk0*hTin
                sinc0     = sin(arg0)/arg0
                Phik(i+0) = Phi0fk*sinc0
                fk1       = fk(i+1)
                arg1      = twopi*fk1*hTin
                sinc1     = sin(arg1)/arg1
                Phik(i+1) = Phi0fk*sinc1
                fk2       = fk(i+2)
                arg2      = twopi*fk2*hTin
                sinc2     = sin(arg2)/arg2
                Phik(i+2) = Phi0fk*sinc2
                fk3       = fk(i+3)
                arg3      = twopi*fk3*hTin
                sinc3     = sin(arg3)/arg3
                Phik(i+3) = Phi0fk*sinc3
                fk4       = fk(i+4)
                arg4      = twopi*fk4*hTin
                sinc4     = sin(arg4)/arg4
                Phik(i+4) = Phi0fk*sinc4
                fk5       = fk(i+5)
                arg5      = twopi*fk5*hTin
                sinc5     = sin(arg5)/arg5
                Phik(i+5) = Phi0fk*sinc5
                fk6       = fk(i+6)
                arg6      = twopi*fk6*hTin
                sinc6     = sin(arg6)/arg6
                Phik(i+6) = Phi0fk*sinc6
                fk7       = fk(i+7)
                arg7      = twopi*fk7*hTin
                sinc7     = sin(arg7)/arg7
                Phik(i+7) = Phi0fk*sinc7
                fk8       = fk(i+8)
                arg8      = twopi*fk8*hTin
                sinc8     = sin(arg8)/arg8
                Phik(i+8) = Phi0fk*sinc8
                fk9       = fk(i+9)
                arg9      = twopi*fk9*hTin
                sinc9     = sin(arg9)/arg9
                Phik(i+9) = Phi0fk*sinc1
                fk10      = fk(i+10)
                arg10     = twopi*fk10*hTin
                sinc10    = sin(arg10)/arg10
                Phik(i+10)= Phi0fk*sinc10
                fk11      = fk(i+11)
                arg11     = twopi*fk11*hTin
                sinc11    = sin(arg11)/arg11
                Phik(i+11)= Phi0fk*sinc3
                fk12      = fk(i+12)
                arg12     = twopi*fk12*hTin
                sinc12    = sin(arg12)/arg12
                Phik(i+12)= Phi0fk*sinc12
                fk13      = fk(i+13)
                arg13     = twopi*fk13*hTin
                sinc13    = sin(arg13)/arg13
                Phik(i+13)= Phi0fk*sinc13
                fk14      = fk(i+14)
                arg14     = twopi*fk14*hTin
                sinc14    = sin(arg14)/arg14
                Phik(i+14)= Phi0fk*sinc14
                fk15      = fk(i+15)
                arg15     = twopi*fk15*hTin
                sinc15    = sin(arg15)/arg15
                Phik(i+15)= Phi0fk*sinc15
           end do
          
        end subroutine rect_pulse_flux_unroll_16x_r8


        subroutine rect_pulse_flux_unroll_8x_r8(Phik,fk,Phi0,n,Tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_flux_unroll_8x_r8
           !dir$ attributes forceinline ::   rect_pulse_flux_unroll_8x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_flux_unroll_8x_r8
           real(kind=dp), dimension(1:n), intent(out) :: Phik
           real(kind=dp), dimension(1:n), intent(in)  :: fk
           real(kind=dp),                 intent(in)  :: Phi0
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp),                 intent(in)  :: Tin
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp) :: fk0,fk1,fk2,fk3,fk4,fk5,fk6,fk7
           real(kind=dp) :: sinc0,sinc1,sinc2,sinc3,sinc4,sinc5,sinc6,sinc7
           real(kind=dp) :: arg0,arg1,arg2,arg3,arg4,arg,arg6,arg7
           real(kind=dp) :: hTin,Phi0fk
           integer(kind=i4) :: i,m,m1
           hTin   = 0.5_dp*Tin
           Phi0fk = Phi0*Tin 
           m=mod(n,8)
           if(m /= 0) then
              do i=1, m
                 fk0       = fk(i)
                 arg0      = twopi*fk0*hTin
                 sinc0     = sin(arg0)/arg0
                 Phik(i)   = Phi0fk*sinc0
              end do
              if(n<8) return
           end if
           m1 = m+1
           !dir$ assume_aligned Phik:64
           !dir$ assume_aligned fk:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,8
                fk0       = fk(i+0)
                arg0      = twopi*fk0*hTin
                sinc0     = sin(arg0)/arg0
                Phik(i+0) = Phi0fk*sinc0
                fk1       = fk(i+1)
                arg1      = twopi*fk1*hTin
                sinc1     = sin(arg1)/arg1
                Phik(i+1) = Phi0fk*sinc1
                fk2       = fk(i+2)
                arg2      = twopi*fk2*hTin
                sinc2     = sin(arg2)/arg2
                Phik(i+2) = Phi0fk*sinc2
                fk3       = fk(i+3)
                arg3      = twopi*fk3*hTin
                sinc3     = sin(arg3)/arg3
                Phik(i+3) = Phi0fk*sinc3
                fk4       = fk(i+4)
                arg4      = twopi*fk4*hTin
                sinc4     = sin(arg4)/arg4
                Phik(i+4) = Phi0fk*sinc4
                fk5       = fk(i+5)
                arg5      = twopi*fk5*hTin
                sinc5     = sin(arg5)/arg5
                Phik(i+5) = Phi0fk*sinc5
                fk6       = fk(i+6)
                arg6      = twopi*fk6*hTin
                sinc6     = sin(arg6)/arg6
                Phik(i+6) = Phi0fk*sinc6
                fk7       = fk(i+7)
                arg7      = twopi*fk7*hTin
                sinc7     = sin(arg7)/arg7
                Phik(i+7) = Phi0fk*sinc7
              
           end do
          
        end subroutine rect_pulse_flux_unroll_8x_r8



        subroutine rect_pulse_flux_unroll_4x_r8(Phik,fk,Phi0,n,Tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_flux_unroll_4x_r8
           !dir$ attributes forceinline ::   rect_pulse_flux_unroll_4x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_flux_unroll_4x_r8
           real(kind=dp), dimension(1:n), intent(out) :: Phik
           real(kind=dp), dimension(1:n), intent(in)  :: fk
           real(kind=dp),                 intent(in)  :: Phi0
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp),                 intent(in)  :: Tin
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp) :: fk0,fk1,fk2,fk3
           real(kind=dp) :: sinc0,sinc1,sinc2,sinc3
           real(kind=dp) :: arg0,arg1,arg2,arg3
           real(kind=dp) :: hTin,Phi0fk
           integer(kind=i4) :: i,m,m1
           hTin   = 0.5_dp*Tin
           Phi0fk = Phi0*Tin 
           m=mod(n,4)
           if(m /= 0) then
              do i=1, m
                 fk0       = fk(i)
                 arg0      = twopi*fk0*hTin
                 sinc0     = sin(arg0)/arg0
                 Phik(i)   = Phi0fk*sinc0
              end do
              if(n<4) return
           end if
           m1 = m+1
           !dir$ assume_aligned Phik:64
           !dir$ assume_aligned fk:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,4
                fk0       = fk(i+0)
                arg0      = twopi*fk0*hTin
                sinc0     = sin(arg0)/arg0
                Phik(i+0) = Phi0fk*sinc0
                fk1       = fk(i+1)
                arg1      = twopi*fk1*hTin
                sinc1     = sin(arg1)/arg1
                Phik(i+1) = Phi0fk*sinc1
                fk2       = fk(i+2)
                arg2      = twopi*fk2*hTin
                sinc2     = sin(arg2)/arg2
                Phik(i+2) = Phi0fk*sinc2
                fk3       = fk(i+3)
                arg3      = twopi*fk3*hTin
                sinc3     = sin(arg3)/arg3
                Phik(i+3) = Phi0fk*sinc3
                            
           end do
          
        end subroutine rect_pulse_flux_unroll_4x_r8


        


        subroutine rect_pulse_flux_unroll_2x_r8(Phik,fk,Phi0,n,Tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_flux_unroll_2x_r8
           !dir$ attributes forceinline ::   rect_pulse_flux_unroll_2x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_flux_unroll_2x_r8
           real(kind=dp), dimension(1:n), intent(out) :: Phik
           real(kind=dp), dimension(1:n), intent(in)  :: fk
           real(kind=dp),                 intent(in)  :: Phi0
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp),                 intent(in)  :: Tin
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp) :: fk0,fk1
           real(kind=dp) :: sinc0,sinc1
           real(kind=dp) :: arg0,arg1
           real(kind=dp) :: hTin,Phi0fk
           integer(kind=i4) :: i,m,m1
           hTin   = 0.5_dp*Tin
           Phi0fk = Phi0*Tin 
           m=mod(n,2)
           if(m /= 0) then
              do i=1, m
                 fk0       = fk(i)
                 arg0      = twopi*fk0*hTin
                 sinc0     = sin(arg0)/arg0
                 Phik(i)   = Phi0fk*sinc0
              end do
              if(n<2) return
           end if
           m1 = m+1
           !dir$ assume_aligned Phik:64
           !dir$ assume_aligned fk:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m1,n,4
                fk0       = fk(i+0)
                arg0      = twopi*fk0*hTin
                sinc0     = sin(arg0)/arg0
                Phik(i+0) = Phi0fk*sinc0
                fk1       = fk(i+1)
                arg1      = twopi*fk1*hTin
                sinc1     = sin(arg1)/arg1
                Phik(i+1) = Phi0fk*sinc1
                                          
           end do
          
        end subroutine rect_pulse_flux_unroll_2x_r8


        subroutine rect_pulse_flux_r8(Phik,fk,Phi0,n,Tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_flux_r8
           !dir$ attributes forceinline ::   rect_pulse_flux_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_flux_r8
           real(kind=dp), dimension(1:n), intent(out) :: Phik
           real(kind=dp), dimension(1:n), intent(in)  :: fk
           real(kind=dp),                 intent(in)  :: Phi0
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp),                 intent(in)  :: Tin
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp) :: fk0
           real(kind=dp) :: sinc0
           real(kind=dp) :: arg0
           real(kind=dp) :: hTin,Phi0fk
           integer(kind=i4) :: i
           hTin   = 0.5_dp*Tin
           Phi0fk = Phi0*Tin 
          
           !dir$ assume_aligned Phik:64
           !dir$ assume_aligned fk:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=1,n
                fk0       = fk(i+0)
                arg0      = twopi*fk0*hTin
                sinc0     = sin(arg0)/arg0
                Phik(i+0) = Phi0fk*sinc0
             
           end do
          
        end subroutine rect_pulse_flux_r8 


        subroutine rect_pulse_flux_exec_r4(Phik,fk,Phi0,n,Tin,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: rect_pulse_flux_exec_r4
           real(kind=sp), dimension(1:n), intent(out) :: Phik
           real(kind=sp), dimension(1:n), intent(in)  :: fk
           real(kind=sp),                 intent(in)  :: Phi0
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp),                 intent(in)  :: Tin
           integer(kind=i4),              intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                call rect_pulse_flux_unroll_16x_r4(Phik,fk,Phi0,n,Tin)
              case (8)
                call rect_pulse_flux_unroll_8x_r4(Phik,fk,Phi0,n,Tin)
              case (4)
                call rect_pulse_flux_unroll_4x_r4(Phik,fk,Phi0,n,Tin)
              case (2)
                call rect_pulse_flux_unroll_2x_r4(Phik,fk,Phi0,n,Tin)
              case (0)
                call rect_pulse_flux_r4(Phik,fk,Phi0,n,Tin)
              case default
                return
              end select
        end subroutine rect_pulse_flux_exec_r4


        subroutine rect_pulse_flux_exec_r8(Phik,fk,Phi0,n,Tin,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: rect_pulse_flux_exec_r8
           real(kind=dp), dimension(1:n), intent(out) :: Phik
           real(kind=dp), dimension(1:n), intent(in)  :: fk
           real(kind=dp),                 intent(in)  :: Phi0
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp),                 intent(in)  :: Tin
           integer(kind=i4),              intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                call rect_pulse_flux_unroll_16x_r8(Phik,fk,Phi0,n,Tin)
              case (8)
                call rect_pulse_flux_unroll_8x_r8(Phik,fk,Phi0,n,Tin)
              case (4)
                call rect_pulse_flux_unroll_4x_r8(Phik,fk,Phi0,n,Tin)
              case (2)
                call rect_pulse_flux_unroll_2x_r8(Phik,fk,Phi0,n,Tin)
              case (0)
                call rect_pulse_flux_r8(Phik,fk,Phi0,n,Tin)
              case default
                return
              end select
        end subroutine rect_pulse_flux_exec_r8

      

        subroutine rect_pulse_amp_unroll_16x_r4(Ak,Phik,Phi0,n,T,k,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_amp_unroll_16x_r4
           !dir$ attributes forceinline ::   rect_pulse_amp_unroll_16x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_amp_unroll_16x_r4
           use mod_fpcompare, only : Compare_Float
           real(kind=sp), dimension(1:n),  intent(out) :: Ak
           real(kind=sp), dimension(1:n),  intent(in)  :: Phik
           real(kind=sp),                  intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           real(kind=sp),                  intent(in)  :: T
           real(kind=sp), dimension(1:n),  intent(in)  :: k
           real(kind=sp),                  intent(in)  :: tin
           real(kind=sp), parameter :: pi2 = 1.5707963267948966192313216916398_sp
           real(kind=sp) :: phik0,phik1,phik2,phik3,phik4,phik5,phik6,phik7
           real(kind=sp) :: phik8,phik9,phik10,phik11,phik12,phik13,phik14,phik15
           real(kind=sp) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           real(kind=sp) :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           real(kind=sp) :: k0,k1,k2,k3,k4,k5,k6,k7
           real(kind=sp) :: k8,k9,k10,k11,k12,k13,k14,k15
           real(kind=sp) :: twoT,kpi2
           integer(kind=i4) :: i,m,m1
           twoT = 2.0_sp/T
           if(Compare_Float(tin,twoT)) then
               m=mod(n,16)
               if(m /= 0) then
                  do i=1,m
                      k0    = k(i)
                      arg0  = k0*pi2
                      Ak(i) = Phi0*(sin(arg0)/arg0) 
                  end do
                  if(n<16) return
                end if
                m1 = m+1
               !dir$ assume_aligned Ak:64
               !dir$ assume_aligned k:64
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector multiple_gather_scatter_by_shuffles 
               !dir$ vector always
               do i=m1,n,16
                    k0      = k(i+0)
                    arg0    = k0*pi2
                    Ak(i+0) = Phi0*(sin(arg0)/arg0) 
                    k1      = k(i+1)
                    arg1    = k1*pi2
                    Ak(i+1) = Phi0*(sin(arg1)/arg1) 
                    k2      = k(i+2)
                    arg2    = k2*pi2
                    Ak(i+2) = Phi0*(sin(arg2)/arg2) 
                    k3      = k(i+3)
                    arg3    = k3*pi2
                    Ak(i+3) = Phi0*(sin(arg3)/arg3) 
                    k4      = k(i+4)
                    arg4    = k4*pi2
                    Ak(i+4) = Phi0*(sin(arg4)/arg4) 
                    k5      = k(i+5)
                    arg5    = k5*pi2
                    Ak(i+5) = Phi0*(sin(arg5)/arg5) 
                    k6      = k(i+6)
                    arg6    = k6*pi2
                    Ak(i+6) = Phi0*(sin(arg6)/arg6)
                    k7      = k(i+7)
                    arg7    = k7*pi2
                    Ak(i+7) = Phi0*(sin(arg7)/arg7)   
                    k08     = k(i+8)
                    arg8    = k8*pi2
                    Ak(i+8) = Phi0*(sin(arg8)/arg8) 
                    k9      = k(i+9)
                    arg9    = k9*pi2
                    Ak(i+9) = Phi0*(sin(arg9)/arg9) 
                    k10     = k(i+10)
                    arg10   = k10*pi2
                    Ak(i+10)= Phi0*(sin(arg10)/arg10) 
                    k11     = k(i+11)
                    arg11   = k11*pi2
                    Ak(i+11)= Phi0*(sin(arg11)/arg11) 
                    k12     = k(i+12)
                    arg12   = k12*pi2
                    Ak(i+12)= Phi0*(sin(arg12)/arg12) 
                    k13     = k(i+13)
                    arg13   = k13*pi2
                    Ak(i+13)= Phi0*(sin(arg13)/arg13) 
                    k14     = k(i+14)
                    arg14   = k14*pi2
                    Ak(i+14)= Phi0*(sin(arg14)/arg14)
                    k15     = k(i+15)
                    arg155  = k15*pi2
                    Ak(i+15)= Phi0*(sin(arg15)/arg15)   
               end do
             
           else
               m=mod(n,16)
               if(m /= 0) then
                  do i=1,m
                      phik0 = Phik(i)
                      Ak(i) = twoT*phik0
                  end do
                  if(n<16) return
                end if
               m1 = m+1
               !dir$ assume_aligned Ak:64
               !dir$ assume_aligned Phik:64
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector multiple_gather_scatter_by_shuffles 
               !dir$ vector always
               do i=m1,n,16
                    phik0   = Phik(i+0)
                    Ak(i+0) = twoT*phik0
                    phik1   = Phik(i+1)
                    Ak(i+1) = twoT*phik1
                    phik2   = Phik(i+2)
                    Ak(i+2) = twoT*phik2
                    phik3   = Phik(i+3)
                    Ak(i+3) = twoT*phik3
                    phik4   = Phik(i+4)
                    Ak(i+4) = twoT*phik4
                    phik5   = Phik(i+5)
                    Ak(i+5) = twoT*phik5
                    phik6   = Phik(i+6)
                    Ak(i+6) = twoT*phik6
                    phik7   = Phik(i+7)
                    Ak(i+7) = twoT*phik7
                    phik8   = Phik(i+8)
                    Ak(i+8) = twoT*phik8
                    phik9   = Phik(i+9)
                    Ak(i+9) = twoT*phik9
                    phik10  = Phik(i+10)
                    Ak(i+10)= twoT*phik10
                    phik11  = Phik(i+11)
                    Ak(i+11)= twoT*phik11
                    phik12  = Phik(i+12)
                    Ak(i+12)= twoT*phik12
                    phik13  = Phik(i+13)
                    Ak(i+13)= twoT*phik13
                    phik14  = Phik(i+14)
                    Ak(i+14)= twoT*phik14
                    phik15  = Phik(i+15)
                    Ak(i+15) = twoT*phik15
              end do
             
          end if
       end subroutine rect_pulse_amp_unroll_16x_r4


       subroutine rect_pulse_amp_unroll_8x_r4(Ak,Phik,Phi0,n,T,k,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_amp_unroll_8x_r4
           !dir$ attributes forceinline ::   rect_pulse_amp_unroll_8x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_amp_unroll_8x_r4
           use mod_fpcompare, only : Compare_Float
           real(kind=sp), dimension(1:n),  intent(out) :: Ak
           real(kind=sp), dimension(1:n),  intent(in)  :: Phik
           real(kind=sp),                  intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           real(kind=sp),                  intent(in)  :: T
           real(kind=sp), dimension(1:n),  intent(in)  :: k
           real(kind=sp),                  intent(in)  :: tin
           real(kind=sp), parameter :: pi2 = 1.5707963267948966192313216916398_sp
           real(kind=sp) :: phik0,phik1,phik2,phik3,phik4,phik5,phik6,phik7
           real(kind=sp) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           real(kind=sp) :: k0,k1,k2,k3,k4,k5,k6,k7
           real(kind=sp) :: twoT,kpi2
           integer(kind=i4) :: i,m,m1
           twoT = 2.0_sp/T
           if(Compare_Float(tin,twoT)) then
               m=mod(n,8)
               if(m /= 0) then
                  do i=1,m
                      k0    = k(i)
                      arg0  = k0*pi2
                      Ak(i) = Phi0*(sin(arg0)/arg0) 
                  end do
                  if(n<8) return
                end if
                m1 = m+1
               !dir$ assume_aligned Ak:64
               !dir$ assume_aligned k:64
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector multiple_gather_scatter_by_shuffles 
               !dir$ vector always
               do i=m1,n,8
                    k0      = k(i+0)
                    arg0    = k0*pi2
                    Ak(i+0) = Phi0*(sin(arg0)/arg0) 
                    k1      = k(i+1)
                    arg1    = k1*pi2
                    Ak(i+1) = Phi0*(sin(arg1)/arg1) 
                    k2      = k(i+2)
                    arg2    = k2*pi2
                    Ak(i+2) = Phi0*(sin(arg2)/arg2) 
                    k3      = k(i+3)
                    arg3    = k3*pi2
                    Ak(i+3) = Phi0*(sin(arg3)/arg3) 
                    k4      = k(i+4)
                    arg4    = k4*pi2
                    Ak(i+4) = Phi0*(sin(arg4)/arg4) 
                    k5      = k(i+5)
                    arg5    = k5*pi2
                    Ak(i+5) = Phi0*(sin(arg5)/arg5) 
                    k6      = k(i+6)
                    arg6    = k6*pi2
                    Ak(i+6) = Phi0*(sin(arg6)/arg6)
                    k7      = k(i+7)
                    arg7    = k7*pi2
                    Ak(i+7) = Phi0*(sin(arg7)/arg7)   
                end do
             
           else
               m=mod(n,8)
               if(m /= 0) then
                  do i=1,m
                      phik0 = Phik(i)
                      Ak(i) = twoT*phik0
                  end do
                  if(n<8) return
                end if
               m1 = m+1
               !dir$ assume_aligned Ak:64
               !dir$ assume_aligned Phik:64
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector multiple_gather_scatter_by_shuffles 
               !dir$ vector always
               do i=m1,n,8
                    phik0   = Phik(i+0)
                    Ak(i+0) = twoT*phik0
                    phik1   = Phik(i+1)
                    Ak(i+1) = twoT*phik1
                    phik2   = Phik(i+2)
                    Ak(i+2) = twoT*phik2
                    phik3   = Phik(i+3)
                    Ak(i+3) = twoT*phik3
                    phik4   = Phik(i+4)
                    Ak(i+4) = twoT*phik4
                    phik5   = Phik(i+5)
                    Ak(i+5) = twoT*phik5
                    phik6   = Phik(i+6)
                    Ak(i+6) = twoT*phik6
                    phik7   = Phik(i+7)
                    Ak(i+7) = twoT*phik7
                   
              end do
             
          end if
       end subroutine rect_pulse_amp_unroll_8x_r4


       subroutine rect_pulse_amp_unroll_4x_r4(Ak,Phik,Phi0,n,T,k,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_amp_unroll_4x_r4
           !dir$ attributes forceinline ::   rect_pulse_amp_unroll_4x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_amp_unroll_4x_r4
           use mod_fpcompare, only : Compare_Float
           real(kind=sp), dimension(1:n),  intent(out) :: Ak
           real(kind=sp), dimension(1:n),  intent(in)  :: Phik
           real(kind=sp),                  intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           real(kind=sp),                  intent(in)  :: T
           real(kind=sp), dimension(1:n),  intent(in)  :: k
           real(kind=sp),                  intent(in)  :: tin
           real(kind=sp), parameter :: pi2 = 1.5707963267948966192313216916398_sp
           real(kind=sp) :: phik0,phik1,phik2,phik3
           real(kind=sp) :: arg0,arg1,arg2,arg3
           real(kind=sp) :: k0,k1,k2,k3
           real(kind=sp) :: twoT,kpi2
           integer(kind=i4) :: i,m,m1
           twoT = 2.0_sp/T
           if(Compare_Float(tin,twoT)) then
               m=mod(n,4)
               if(m /= 0) then
                  do i=1,m
                      k0    = k(i)
                      arg0  = k0*pi2
                      Ak(i) = Phi0*(sin(arg0)/arg0) 
                  end do
                  if(n<4) return
                end if
                m1 = m+1
               !dir$ assume_aligned Ak:64
               !dir$ assume_aligned k:64
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector multiple_gather_scatter_by_shuffles 
               !dir$ vector always
               do i=m1,n,4
                    k0      = k(i+0)
                    arg0    = k0*pi2
                    Ak(i+0) = Phi0*(sin(arg0)/arg0) 
                    k1      = k(i+1)
                    arg1    = k1*pi2
                    Ak(i+1) = Phi0*(sin(arg1)/arg1) 
                    k2      = k(i+2)
                    arg2    = k2*pi2
                    Ak(i+2) = Phi0*(sin(arg2)/arg2) 
                    k3      = k(i+3)
                    arg3    = k3*pi2
                    Ak(i+3) = Phi0*(sin(arg3)/arg3) 
                 
                end do
             
           else
               m=mod(n,4)
               if(m /= 0) then
                  do i=1,m
                      phik0 = Phik(i)
                      Ak(i) = twoT*phik0
                  end do
                  if(n<4) return
                end if
               m1 = m+1
               !dir$ assume_aligned Ak:64
               !dir$ assume_aligned Phik:64
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector multiple_gather_scatter_by_shuffles 
               !dir$ vector always
               do i=m1,n,4
                    phik0   = Phik(i+0)
                    Ak(i+0) = twoT*phik0
                    phik1   = Phik(i+1)
                    Ak(i+1) = twoT*phik1
                    phik2   = Phik(i+2)
                    Ak(i+2) = twoT*phik2
                    phik3   = Phik(i+3)
                    Ak(i+3) = twoT*phik3
                                     
              end do
             
          end if
       end subroutine rect_pulse_amp_unroll_4x_r4


       subroutine rect_pulse_amp_unroll_2x_r4(Ak,Phik,Phi0,n,T,k,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_amp_unroll_2x_r4
           !dir$ attributes forceinline ::   rect_pulse_amp_unroll_2x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_amp_unroll_2x_r4
           use mod_fpcompare, only : Compare_Float
           real(kind=sp), dimension(1:n),  intent(out) :: Ak
           real(kind=sp), dimension(1:n),  intent(in)  :: Phik
           real(kind=sp),                  intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           real(kind=sp),                  intent(in)  :: T
           real(kind=sp), dimension(1:n),  intent(in)  :: k
           real(kind=sp),                  intent(in)  :: tin
           real(kind=sp), parameter :: pi2 = 1.5707963267948966192313216916398_sp
           real(kind=sp) :: phik0,phik1
           real(kind=sp) :: arg0,arg1
           real(kind=sp) :: k0,k1
           real(kind=sp) :: twoT,kpi2
           integer(kind=i4) :: i,m,m1
           twoT = 2.0_sp/T
           if(Compare_Float(tin,twoT)) then
               m=mod(n,2)
               if(m /= 0) then
                  do i=1,m
                      k0    = k(i)
                      arg0  = k0*pi2
                      Ak(i) = Phi0*(sin(arg0)/arg0) 
                  end do
                  if(n<2) return
                end if
                m1 = m+1
               !dir$ assume_aligned Ak:64
               !dir$ assume_aligned k:64
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector multiple_gather_scatter_by_shuffles 
               !dir$ vector always
               do i=m1,n,2
                    k0      = k(i+0)
                    arg0    = k0*pi2
                    Ak(i+0) = Phi0*(sin(arg0)/arg0) 
                    k1      = k(i+1)
                    arg1    = k1*pi2
                    Ak(i+1) = Phi0*(sin(arg1)/arg1) 
               end do
             
           else
               m=mod(n,2)
               if(m /= 0) then
                  do i=1,m
                      phik0 = Phik(i)
                      Ak(i) = twoT*phik0
                  end do
                  if(n<2) return
                end if
               m1 = m+1
               !dir$ assume_aligned Ak:64
               !dir$ assume_aligned Phik:64
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector multiple_gather_scatter_by_shuffles 
               !dir$ vector always
               do i=m1,n,2
                    phik0   = Phik(i+0)
                    Ak(i+0) = twoT*phik0
                    phik1   = Phik(i+1)
                    Ak(i+1) = twoT*phik1
                                                        
              end do
             
          end if
       end subroutine rect_pulse_amp_unroll_2x_r4



       subroutine rect_pulse_amp_unroll_16x_r8(Ak,Phik,Phi0,n,T,k,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_amp_unroll_16x_r8
           !dir$ attributes forceinline ::   rect_pulse_amp_unroll_16x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_amp_unroll_16x_r8
           use mod_fpcompare, only : Compare_Float
           real(kind=dp), dimension(1:n),  intent(out) :: Ak
           real(kind=dp), dimension(1:n),  intent(in)  :: Phik
           real(kind=dp),                  intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           real(kind=dp),                  intent(in)  :: T
           real(kind=dp), dimension(1:n),  intent(in)  :: k
           real(kind=dp),                  intent(in)  :: tin
           real(kind=dp), parameter :: pi2 = 1.5707963267948966192313216916398_dp
           real(kind=dp) :: phik0,phik1,phik2,phik3,phik4,phik5,phik6,phik7
           real(kind=dp) :: phik8,phik9,phik10,phik11,phik12,phik13,phik14,phik15
           real(kind=dp) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           real(kind=dp) :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           real(kind=dp) :: k0,k1,k2,k3,k4,k5,k6,k7
           real(kind=dp) :: k8,k9,k10,k11,k12,k13,k14,k15
           real(kind=dp) :: twoT,kpi2
           integer(kind=i4) :: i,m,m1
           twoT = 2.0_dp/T
           if(Compare_Float(tin,twoT)) then
               m=mod(n,16)
               if(m /= 0) then
                  do i=1,m
                      k0    = k(i)
                      arg0  = k0*pi2
                      Ak(i) = Phi0*(sin(arg0)/arg0) 
                  end do
                  if(n<16) return
                end if
                m1 = m+1
               !dir$ assume_aligned Ak:64
               !dir$ assume_aligned k:64
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(8)
               !dir$ vector multiple_gather_scatter_by_shuffles 
               !dir$ vector always
               do i=m1,n,16
                    k0      = k(i+0)
                    arg0    = k0*pi2
                    Ak(i+0) = Phi0*(sin(arg0)/arg0) 
                    k1      = k(i+1)
                    arg1    = k1*pi2
                    Ak(i+1) = Phi0*(sin(arg1)/arg1) 
                    k2      = k(i+2)
                    arg2    = k2*pi2
                    Ak(i+2) = Phi0*(sin(arg2)/arg2) 
                    k3      = k(i+3)
                    arg3    = k3*pi2
                    Ak(i+3) = Phi0*(sin(arg3)/arg3) 
                    k4      = k(i+4)
                    arg4    = k4*pi2
                    Ak(i+4) = Phi0*(sin(arg4)/arg4) 
                    k5      = k(i+5)
                    arg5    = k5*pi2
                    Ak(i+5) = Phi0*(sin(arg5)/arg5) 
                    k6      = k(i+6)
                    arg6    = k6*pi2
                    Ak(i+6) = Phi0*(sin(arg6)/arg6)
                    k7      = k(i+7)
                    arg7    = k7*pi2
                    Ak(i+7) = Phi0*(sin(arg7)/arg7)   
                    k08     = k(i+8)
                    arg8    = k8*pi2
                    Ak(i+8) = Phi0*(sin(arg8)/arg8) 
                    k9      = k(i+9)
                    arg9    = k9*pi2
                    Ak(i+9) = Phi0*(sin(arg9)/arg9) 
                    k10     = k(i+10)
                    arg10   = k10*pi2
                    Ak(i+10)= Phi0*(sin(arg10)/arg10) 
                    k11     = k(i+11)
                    arg11   = k11*pi2
                    Ak(i+11)= Phi0*(sin(arg11)/arg11) 
                    k12     = k(i+12)
                    arg12   = k12*pi2
                    Ak(i+12)= Phi0*(sin(arg12)/arg12) 
                    k13     = k(i+13)
                    arg13   = k13*pi2
                    Ak(i+13)= Phi0*(sin(arg13)/arg13) 
                    k14     = k(i+14)
                    arg14   = k14*pi2
                    Ak(i+14)= Phi0*(sin(arg14)/arg14)
                    k15     = k(i+15)
                    arg155  = k15*pi2
                    Ak(i+15)= Phi0*(sin(arg15)/arg15)   
               end do
             
           else
               m=mod(n,16)
               if(m /= 0) then
                  do i=1,m
                      phik0 = Phik(i)
                      Ak(i) = twoT*phik0
                  end do
                  if(n<16) return
                end if
               m1 = m+1
               !dir$ assume_aligned Ak:64
               !dir$ assume_aligned Phik:64
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(8)
               !dir$ vector multiple_gather_scatter_by_shuffles 
               !dir$ vector always
               do i=m1,n,16
                    phik0   = Phik(i+0)
                    Ak(i+0) = twoT*phik0
                    phik1   = Phik(i+1)
                    Ak(i+1) = twoT*phik1
                    phik2   = Phik(i+2)
                    Ak(i+2) = twoT*phik2
                    phik3   = Phik(i+3)
                    Ak(i+3) = twoT*phik3
                    phik4   = Phik(i+4)
                    Ak(i+4) = twoT*phik4
                    phik5   = Phik(i+5)
                    Ak(i+5) = twoT*phik5
                    phik6   = Phik(i+6)
                    Ak(i+6) = twoT*phik6
                    phik7   = Phik(i+7)
                    Ak(i+7) = twoT*phik7
                    phik8   = Phik(i+8)
                    Ak(i+8) = twoT*phik8
                    phik9   = Phik(i+9)
                    Ak(i+9) = twoT*phik9
                    phik10  = Phik(i+10)
                    Ak(i+10)= twoT*phik10
                    phik11  = Phik(i+11)
                    Ak(i+11)= twoT*phik11
                    phik12  = Phik(i+12)
                    Ak(i+12)= twoT*phik12
                    phik13  = Phik(i+13)
                    Ak(i+13)= twoT*phik13
                    phik14  = Phik(i+14)
                    Ak(i+14)= twoT*phik14
                    phik15  = Phik(i+15)
                    Ak(i+15) = twoT*phik15
              end do
             
          end if
       end subroutine rect_pulse_amp_unroll_16x_r8


       subroutine rect_pulse_amp_unroll_8x_r8(Ak,Phik,Phi0,n,T,k,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_amp_unroll_8x_r8
           !dir$ attributes forceinline ::   rect_pulse_amp_unroll_8x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_amp_unroll_8x_r8
           use mod_fpcompare, only : Compare_Float
           real(kind=dp), dimension(1:n),  intent(out) :: Ak
           real(kind=dp), dimension(1:n),  intent(in)  :: Phik
           real(kind=dp),                  intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           real(kind=dp),                  intent(in)  :: T
           real(kind=dp), dimension(1:n),  intent(in)  :: k
           real(kind=dp),                  intent(in)  :: tin
           real(kind=dp), parameter :: pi2 = 1.5707963267948966192313216916398_dp
           real(kind=dp) :: phik0,phik1,phik2,phik3,phik4,phik5,phik6,phik7
           real(kind=dp) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           real(kind=dp) :: k0,k1,k2,k3,k4,k5,k6,k7
           real(kind=dp) :: twoT,kpi2
           integer(kind=i4) :: i,m,m1
           twoT = 2.0_dp/T
           if(Compare_Float(tin,twoT)) then
               m=mod(n,8)
               if(m /= 0) then
                  do i=1,m
                      k0    = k(i)
                      arg0  = k0*pi2
                      Ak(i) = Phi0*(sin(arg0)/arg0) 
                  end do
                  if(n<8) return
                end if
                m1 = m+1
               !dir$ assume_aligned Ak:64
               !dir$ assume_aligned k:64
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(8)
               !dir$ vector multiple_gather_scatter_by_shuffles 
               !dir$ vector always
               do i=m1,n,8
                    k0      = k(i+0)
                    arg0    = k0*pi2
                    Ak(i+0) = Phi0*(sin(arg0)/arg0) 
                    k1      = k(i+1)
                    arg1    = k1*pi2
                    Ak(i+1) = Phi0*(sin(arg1)/arg1) 
                    k2      = k(i+2)
                    arg2    = k2*pi2
                    Ak(i+2) = Phi0*(sin(arg2)/arg2) 
                    k3      = k(i+3)
                    arg3    = k3*pi2
                    Ak(i+3) = Phi0*(sin(arg3)/arg3) 
                    k4      = k(i+4)
                    arg4    = k4*pi2
                    Ak(i+4) = Phi0*(sin(arg4)/arg4) 
                    k5      = k(i+5)
                    arg5    = k5*pi2
                    Ak(i+5) = Phi0*(sin(arg5)/arg5) 
                    k6      = k(i+6)
                    arg6    = k6*pi2
                    Ak(i+6) = Phi0*(sin(arg6)/arg6)
                    k7      = k(i+7)
                    arg7    = k7*pi2
                    Ak(i+7) = Phi0*(sin(arg7)/arg7)   
                end do
             
           else
               m=mod(n,8)
               if(m /= 0) then
                  do i=1,m
                      phik0 = Phik(i)
                      Ak(i) = twoT*phik0
                  end do
                  if(n<8) return
                end if
               m1 = m+1
               !dir$ assume_aligned Ak:64
               !dir$ assume_aligned Phik:64
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(8)
               !dir$ vector multiple_gather_scatter_by_shuffles 
               !dir$ vector always
               do i=m1,n,8
                    phik0   = Phik(i+0)
                    Ak(i+0) = twoT*phik0
                    phik1   = Phik(i+1)
                    Ak(i+1) = twoT*phik1
                    phik2   = Phik(i+2)
                    Ak(i+2) = twoT*phik2
                    phik3   = Phik(i+3)
                    Ak(i+3) = twoT*phik3
                    phik4   = Phik(i+4)
                    Ak(i+4) = twoT*phik4
                    phik5   = Phik(i+5)
                    Ak(i+5) = twoT*phik5
                    phik6   = Phik(i+6)
                    Ak(i+6) = twoT*phik6
                    phik7   = Phik(i+7)
                    Ak(i+7) = twoT*phik7
                   
              end do
             
          end if
       end subroutine rect_pulse_amp_unroll_8x_r8


       subroutine rect_pulse_amp_unroll_4x_r8(Ak,Phik,Phi0,n,T,k,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_amp_unroll_4x_r8
           !dir$ attributes forceinline ::   rect_pulse_amp_unroll_4x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_amp_unroll_4x_r8
           use mod_fpcompare, only : Compare_Float
           real(kind=dp), dimension(1:n),  intent(out) :: Ak
           real(kind=dp), dimension(1:n),  intent(in)  :: Phik
           real(kind=dp),                  intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           real(kind=dp),                  intent(in)  :: T
           real(kind=dp), dimension(1:n),  intent(in)  :: k
           real(kind=dp),                  intent(in)  :: tin
           real(kind=dp), parameter :: pi2 = 1.5707963267948966192313216916398_dp
           real(kind=dp) :: phik0,phik1,phik2,phik3
           real(kind=dp) :: arg0,arg1,arg2,arg3
           real(kind=dp) :: k0,k1,k2,k3
           real(kind=dp) :: twoT,kpi2
           integer(kind=i4) :: i,m,m1
           twoT = 2.0_dp/T
           if(Compare_Float(tin,twoT)) then
               m=mod(n,4)
               if(m /= 0) then
                  do i=1,m
                      k0    = k(i)
                      arg0  = k0*pi2
                      Ak(i) = Phi0*(sin(arg0)/arg0) 
                  end do
                  if(n<4) return
                end if
                m1 = m+1
               !dir$ assume_aligned Ak:64
               !dir$ assume_aligned k:64
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(8)
               !dir$ vector multiple_gather_scatter_by_shuffles 
               !dir$ vector always
               do i=m1,n,4
                    k0      = k(i+0)
                    arg0    = k0*pi2
                    Ak(i+0) = Phi0*(sin(arg0)/arg0) 
                    k1      = k(i+1)
                    arg1    = k1*pi2
                    Ak(i+1) = Phi0*(sin(arg1)/arg1) 
                    k2      = k(i+2)
                    arg2    = k2*pi2
                    Ak(i+2) = Phi0*(sin(arg2)/arg2) 
                    k3      = k(i+3)
                    arg3    = k3*pi2
                    Ak(i+3) = Phi0*(sin(arg3)/arg3) 
                end do
             
           else
               m=mod(n,4)
               if(m /= 0) then
                  do i=1,m
                      phik0 = Phik(i)
                      Ak(i) = twoT*phik0
                  end do
                  if(n<4) return
                end if
               m1 = m+1
               !dir$ assume_aligned Ak:64
               !dir$ assume_aligned Phik:64
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(8)
               !dir$ vector multiple_gather_scatter_by_shuffles 
               !dir$ vector always
               do i=m1,n,4
                    phik0   = Phik(i+0)
                    Ak(i+0) = twoT*phik0
                    phik1   = Phik(i+1)
                    Ak(i+1) = twoT*phik1
                    phik2   = Phik(i+2)
                    Ak(i+2) = twoT*phik2
                    phik3   = Phik(i+3)
                    Ak(i+3) = twoT*phik3
                                    
              end do
             
          end if
       end subroutine rect_pulse_amp_unroll_4x_r8


       subroutine rect_pulse_amp_unroll_2x_r4(Ak,Phik,Phi0,n,T,k,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_amp_unroll_2x_r4
           !dir$ attributes forceinline ::   rect_pulse_amp_unroll_2x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_amp_unroll_2x_r4
           use mod_fpcompare, only : Compare_Float
           real(kind=sp), dimension(1:n),  intent(out) :: Ak
           real(kind=sp), dimension(1:n),  intent(in)  :: Phik
           real(kind=sp),                  intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           real(kind=sp),                  intent(in)  :: T
           real(kind=sp), dimension(1:n),  intent(in)  :: k
           real(kind=sp),                  intent(in)  :: tin
           real(kind=sp), parameter :: pi2 = 1.5707963267948966192313216916398_sp
           real(kind=sp) :: phik0,phik1
           real(kind=sp) :: arg0,arg1
           real(kind=sp) :: k0,k1
           real(kind=sp) :: twoT,kpi2
           integer(kind=i4) :: i,m,m1
           twoT = 2.0_sp/T
           if(Compare_Float(tin,twoT)) then
               m=mod(n,2)
               if(m /= 0) then
                  do i=1,m
                      k0    = k(i)
                      arg0  = k0*pi2
                      Ak(i) = Phi0*(sin(arg0)/arg0) 
                  end do
                  if(n<2) return
                end if
                m1 = m+1
               !dir$ assume_aligned Ak:64
               !dir$ assume_aligned k:64
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector multiple_gather_scatter_by_shuffles 
               !dir$ vector always
               do i=m1,n,2
                    k0      = k(i+0)
                    arg0    = k0*pi2
                    Ak(i+0) = Phi0*(sin(arg0)/arg0) 
                    k1      = k(i+1)
                    arg1    = k1*pi2
                    Ak(i+1) = Phi0*(sin(arg1)/arg1) 
                end do
             
           else
               m=mod(n,2)
               if(m /= 0) then
                  do i=1,m
                      phik0 = Phik(i)
                      Ak(i) = twoT*phik0
                  end do
                  if(n<2) return
                end if
               m1 = m+1
               !dir$ assume_aligned Ak:64
               !dir$ assume_aligned Phik:64
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector multiple_gather_scatter_by_shuffles 
               !dir$ vector always
               do i=m1,n,2
                    phik0   = Phik(i+0)
                    Ak(i+0) = twoT*phik0
                    phik1   = Phik(i+1)
                    Ak(i+1) = twoT*phik1
                                                      
              end do
             
          end if
       end subroutine rect_pulse_amp_unroll_2x_r4




       subroutine rect_pulse_amp_unroll_2x_r8(Ak,Phik,Phi0,n,T,k,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_amp_unroll_2x_r8
           !dir$ attributes forceinline ::   rect_pulse_amp_unroll_2x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_amp_unroll_2x_r8
           use mod_fpcompare, only : Compare_Float
           real(kind=dp), dimension(1:n),  intent(out) :: Ak
           real(kind=dp), dimension(1:n),  intent(in)  :: Phik
           real(kind=dp),                  intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           real(kind=dp),                  intent(in)  :: T
           real(kind=dp), dimension(1:n),  intent(in)  :: k
           real(kind=dp),                  intent(in)  :: tin
           real(kind=dp), parameter :: pi2 = 1.5707963267948966192313216916398_dp
           real(kind=dp) :: phik0,phik1
           real(kind=dp) :: arg0,arg1
           real(kind=dp) :: k0,k1
           real(kind=dp) :: twoT,kpi2
           integer(kind=i4) :: i,m,m1
           twoT = 2.0_dp/T
           if(Compare_Float(tin,twoT)) then
               m=mod(n,2)
               if(m /= 0) then
                  do i=1,m
                      k0    = k(i)
                      arg0  = k0*pi2
                      Ak(i) = Phi0*(sin(arg0)/arg0) 
                  end do
                  if(n<2) return
                end if
                m1 = m+1
               !dir$ assume_aligned Ak:64
               !dir$ assume_aligned k:64
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(8)
               !dir$ vector multiple_gather_scatter_by_shuffles 
               !dir$ vector always
               do i=m1,n,2
                    k0      = k(i+0)
                    arg0    = k0*pi2
                    Ak(i+0) = Phi0*(sin(arg0)/arg0) 
                    k1      = k(i+1)
                    arg1    = k1*pi2
                    Ak(i+1) = Phi0*(sin(arg1)/arg1) 
                end do
             
           else
               m=mod(n,2)
               if(m /= 0) then
                  do i=1,m
                      phik0 = Phik(i)
                      Ak(i) = twoT*phik0
                  end do
                  if(n<2) return
                end if
               m1 = m+1
               !dir$ assume_aligned Ak:64
               !dir$ assume_aligned Phik:64
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(8)
               !dir$ vector multiple_gather_scatter_by_shuffles 
               !dir$ vector always
               do i=m1,n,2
                    phik0   = Phik(i+0)
                    Ak(i+0) = twoT*phik0
                    phik1   = Phik(i+1)
                    Ak(i+1) = twoT*phik1
                                                      
              end do
             
          end if
       end subroutine rect_pulse_amp_unroll_2x_r8


       subroutine rect_pulse_amp_r4(Ak,Phik,Phi0,n,T,k,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_amp_r4
           !dir$ attributes forceinline ::   rect_pulse_amp_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_amp_r4
           use mod_fpcompare, only : Compare_Float
           real(kind=sp), dimension(1:n),  intent(out) :: Ak
           real(kind=sp), dimension(1:n),  intent(in)  :: Phik
           real(kind=sp),                  intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           real(kind=sp),                  intent(in)  :: T
           real(kind=sp), dimension(1:n),  intent(in)  :: k
           real(kind=sp),                  intent(in)  :: tin
           real(kind=sp), parameter :: pi2 = 1.5707963267948966192313216916398_sp
           real(kind=sp) :: phik0
           real(kind=sp) :: arg0
           real(kind=sp) :: k0
           real(kind=sp) :: twoT,kpi2
           integer(kind=i4) :: i
           twoT = 2.0_sp/T
           if(Compare_Float(tin,twoT)) then
            
               !dir$ assume_aligned Ak:64
               !dir$ assume_aligned k:64
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector multiple_gather_scatter_by_shuffles 
               !dir$ vector always
               do i=1,n
                    k0      = k(i+0)
                    arg0    = k0*pi2
                    Ak(i+0) = Phi0*(sin(arg0)/arg0) 
               end do
             
           else
              
               !dir$ assume_aligned Ak:64
               !dir$ assume_aligned Phik:64
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector multiple_gather_scatter_by_shuffles 
               !dir$ vector always
               do i=1,n
                    phik0   = Phik(i+0)
                    Ak(i+0) = twoT*phik0
                                                                         
              end do
             
          end if
       end subroutine rect_pulse_amp_r4


      


       subroutine rect_pulse_amp_r8(Ak,Phik,Phi0,n,T,k,tin)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  rect_pulse_amp_r8
           !dir$ attributes forceinline ::   rect_pulse_amp_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  rect_pulse_amp_r8
           use mod_fpcompare, only : Compare_Float
           real(kind=dp), dimension(1:n),  intent(out) :: Ak
           real(kind=dp), dimension(1:n),  intent(in)  :: Phik
           real(kind=dp),                  intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           real(kind=dp),                  intent(in)  :: T
           real(kind=dp), dimension(1:n),  intent(in)  :: k
           real(kind=dp),                  intent(in)  :: tin
           real(kind=dp), parameter :: pi2 = 1.5707963267948966192313216916398_dp
           real(kind=dp) :: phik0
           real(kind=dp) :: arg0
           real(kind=dp) :: k0
           real(kind=dp) :: twoT,kpi2
           integer(kind=i4) :: i
           twoT = 2.0_dp/T
           if(Compare_Float(tin,twoT)) then
            
               !dir$ assume_aligned Ak:64
               !dir$ assume_aligned k:64
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(8)
               !dir$ vector multiple_gather_scatter_by_shuffles 
               !dir$ vector always
               do i=1,n
                    k0      = k(i+0)
                    arg0    = k0*pi2
                    Ak(i+0) = Phi0*(sin(arg0)/arg0) 
               end do
             
           else
              
               !dir$ assume_aligned Ak:64
               !dir$ assume_aligned Phik:64
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(8)
               !dir$ vector multiple_gather_scatter_by_shuffles 
               !dir$ vector always
               do i=1,n
                    phik0   = Phik(i+0)
                    Ak(i+0) = twoT*phik0
                                                                         
              end do
             
          end if
       end subroutine rect_pulse_amp_r8


       subroutine rect_pulse_amp_r4(Ak,Phik,Phi0,n,T,k,tin,unroll_cnt)
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: rect_pulse_amp_r4
           real(kind=sp), dimension(1:n),  intent(out) :: Ak
           real(kind=sp), dimension(1:n),  intent(in)  :: Phik
           real(kind=sp),                  intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           real(kind=sp),                  intent(in)  :: T
           real(kind=sp), dimension(1:n),  intent(in)  :: k
           real(kind=sp),                  intent(in)  :: tin
           integer(kind=i4),               intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                call rect_pulse_amp_unroll_16x_r4(Ak,Phik,Phi0,n,T,k,tin)
              case (8)
                call rect_pulse_amp_unroll_8x_r4(Ak,Phik,Phi0,n,T,k,tin)
              case (4)
                call rect_pulse_amp_unroll_4x_r4(Ak,Phik,Phi0,n,T,k,tin)
              case (2)
                call rect_pulse_amp_unroll_2x_r4(Ak,Phik,Phi0,n,T,k,tin)
              case (0)
                call rect_pulse_amp_r4(Ak,Phik,Phi0,n,T,k,tin)
              case default
                return
              end select
       end subroutine rect_pulse_amp_exec_r4


       subroutine rect_pulse_amp_r8(Ak,Phik,Phi0,n,T,k,tin,unroll_cnt)
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: rect_pulse_amp_r8
           real(kind=dp), dimension(1:n),  intent(out) :: Ak
           real(kind=dp), dimension(1:n),  intent(in)  :: Phik
           real(kind=dp),                  intent(in)  :: Phi0
           integer(kind=i4),               intent(in)  :: n
           real(kind=dp),                  intent(in)  :: T
           real(kind=dp), dimension(1:n),  intent(in)  :: k
           real(kind=dp),                  intent(in)  :: tin
           integer(kind=i4),               intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                call rect_pulse_amp_unroll_16x_r8(Ak,Phik,Phi0,n,T,k,tin)
              case (8)
                call rect_pulse_amp_unroll_8x_r8(Ak,Phik,Phi0,n,T,k,tin)
              case (4)
                call rect_pulse_amp_unroll_4x_r8(Ak,Phik,Phi0,n,T,k,tin)
              case (2)
                call rect_pulse_amp_unroll_2x_r8(Ak,Phik,Phi0,n,T,k,tin)
              case (0)
                call rect_pulse_amp_r8(Ak,Phik,Phi0,n,T,k,tin)
              case default
                return
              end select
       end subroutine rect_pulse_amp_exec_r8
  
       !разность полярных углов ф и ф', 
       !характеризующих положение центра отверстия растра относительно
       !точек О и О',
       !Formula 6, p. 191
       pure elemental function polar_ang_diff_r4(a,R0,phi1,phi2) result(dft)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  polar_ang_diff_r4
           !dir$ attributes forceinline ::   polar_ang_diff_r4
           real(kind=sp),    intent(in) :: a
           real(kind=sp),    intent(in) :: R0
           real(kind=sp),    intent(in) :: phi1
           real(kind=sp),    intent(in) :: phi2
           real(kind=sp) :: dft
           real(kind=sp),automatic :: t0,t1
           t0 = a/R0
           t1 = sin(phi1)-sin(phi2)
           dft= t0*t1
       end function polar_ang_diff_r4

       
       pure elemental function polar_ang_diff_r8(a,R0,phi1,phi2) result(dft)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  polar_ang_diff_r8
           !dir$ attributes forceinline ::   polar_ang_diff_r8
           real(kind=dp),    intent(in) :: a
           real(kind=dp),    intent(in) :: R0
           real(kind=dp),    intent(in) :: phi1
           real(kind=dp),    intent(in) :: phi2
           real(kind=dp) :: dft
           real(kind=dp),automatic :: t0,t1
           t0 = a/R0
           t1 = sin(phi1)-sin(phi2)
           dft= t0*t1
       end function polar_ang_diff_r8


       pure elemental function modulation_freq_diff_r4(phi1,phi2,a,R0,f0) result(dom)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 ::  modulation_freq_diff_r4
             !dir$ attributes forceinline ::   modulation_freq_diff_r4
             real(kind=sp),   intent(in) ::  phi1
             real(kind=sp),   intent(in) ::  phi2
             real(kind=sp),   intent(in) ::  a
             real(kind=sp),   intent(in) ::  R0
             real(kind=sp),   intent(in) ::  f0
             real(kind=sp) :: dom
             real(kind=sp), automatic :: t0,t1,t2
             t0 = 0.5_sp*(phi1+phi2)
             t1 = f0*(a/R0)
             t2 = cos(t0)
             dom= t1*t2
       end function modulation_freq_diff_r4


       pure elemental function modulation_freq_diff_r8(phi1,phi2,a,R0,f0) result(dom)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 ::  modulation_freq_diff_r8
             !dir$ attributes forceinline ::   modulation_freq_diff_r8
             real(kind=dp),   intent(in) ::  phi1
             real(kind=dp),   intent(in) ::  phi2
             real(kind=dp),   intent(in) ::  a
             real(kind=dp),   intent(in) ::  R0
             real(kind=dp),   intent(in) ::  f0
             real(kind=dp) :: dom
             real(kind=dp), automatic :: t0,t1,t2
             t0 = 0.5_dp*(phi1+phi2)
             t1 = f0*(a/R0)
             t2 = cos(t0)
             dom= t1*t2
       end function modulation_freq_diff_r8


       !Formula 12, p. 191
       pure elemental function instantenous_freq_r4(phi1,phi2,a,R0,f0) result(omt)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 ::  instantenous_freq_r4
             !dir$ attributes forceinline ::   instantenous_freq_r4
             real(kind=sp),   intent(in) ::  phi1
             real(kind=sp),   intent(in) ::  phi2
             real(kind=sp),   intent(in) ::  a
             real(kind=sp),   intent(in) ::  R0
             real(kind=sp),   intent(in) ::  f0
             real(kind=sp) :: omt
             real(kind=sp),automatic :: t0
             t0 = modulation_freq_diff_r4(phi1,phi2,a,R0,f0)
             omt= f0+t0
       end function instantenous_freq_r4


       pure elemental function instantenous_freq_r8(phi1,phi2,a,R0,f0) result(omt)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 ::  instantenous_freq_r8
             !dir$ attributes forceinline ::   instantenous_freq_r8
             real(kind=dp),   intent(in) ::  phi1
             real(kind=dp),   intent(in) ::  phi2
             real(kind=dp),   intent(in) ::  a
             real(kind=dp),   intent(in) ::  R0
             real(kind=dp),   intent(in) ::  f0
             real(kind=dp) :: omt
             real(kind=dp),automatic :: t0
             t0 = modulation_freq_diff_r8(phi1,phi2,a,R0,f0)
             omt= f0+t0
       end function instantenous_freq_r8


       !то набег полной фазы за время t
       !Formula 3, p. 192
       ! Omega0 = 2*PI*n
       ! omd    = 2*PI*fd
       ! 
       pure elemental function full_phase_analytic_r4(om0,t,omd,Omega0) result(phase)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 ::  full_phase_analytic_r4
             !dir$ attributes forceinline ::   full_phase_analytic_r4
             real(kind=sp),  intent(in) :: om0
             real(kind=sp),  intent(in) :: t,
             real(kind=sp),  intent(in) :: omd
             real(kind=sp),  intent(in) :: Omega0
             real(kind=sp) :: phase
             real(kind=sp), automatic :: t0,t1,t2,rterm
             t0    = om0*t
             t1    = omd/Omega0
             t2    = sin(Omega0*t)
             rterm = t0+t1*t2
             phase = rterm
       end function full_phase_analytic_r4


       
       pure elemental function full_phase_analytic_r8(om0,t,omd,Omega0) result(phase)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 ::  full_phase_analytic_r8
             !dir$ attributes forceinline ::   full_phase_analytic_r8
             real(kind=dp),  intent(in) :: om0
             real(kind=dp),  intent(in) :: t,
             real(kind=dp),  intent(in) :: omd
             real(kind=dp),  intent(in) :: Omega0
             real(kind=dp) :: phase
             real(kind=dp), automatic :: t0,t1,t2,rterm
             t0    = om0*t
             t1    = omd/Omega0
             t2    = sin(Omega0*t)
             rterm = t0+t1*t2
             phase = rterm
       end function full_phase_analytic_r8


       !коэффициент пропускания 
       !Formula 4, p. 192
       subroutine transmit_coef_unroll_16x_r4(rho0,rho1,rhot,n,Beta,Omega0,om0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmit_coef_unroll_16x_r4
           !dir$ attributes forceinline ::   transmit_coef_unroll_16x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmit_coef_unroll_16x_r4
           real(kind=sp),                 intent(in)  :: rho0
           real(kind=sp),                 intent(in)  :: rho1
           real(kind=sp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp),                 intent(in)  :: Beta
           real(kind=sp),                 intent(in)  :: Omega0
           real(kind=sp),                 intent(in)  :: om0
           real(kind=sp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=sp), automatic :: t8,t9,t10,t11,t12,t13,t14,t15
           real(kind=sp), automatic :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           real(kind=sp), automatic :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           real(kind=sp), automatic :: sin0,sin1,sin2,sin3,sin4,sin5,sin6,sin7
           real(kind=sp), automatic :: sin8,sin9,sin10,sin11,sin12,sin13,sin14,sin15
           real(kind=sp), automatic :: tmp0,tmp1,tmp2,tmp3,tmp4,tmp,tmp6,tmp7
           real(kind=sp), automatic :: tmp8,tmp9,tmp10,tmp11,tmp12,tmp13,tmp14,tmp15
           integer(kind=i4) :: i,m,m1
           m = mod(n,16)
           if(m /= 0) then
              do i=1,m
                 t0      = real(i,kind=sp)
                 arg0    = om0*t0
                 sin0    = Beta*sin(Omega0*t0)
                 tmp0    = sin(arg0+sin0)
                 rhot(i) = rho0+rho1*tmp0
              end do
              if(n<16) return
            end if 
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,16
                t0        = real(i,kind=sp)
                arg0      = om0*t0
                sin0      = Beta*sin(Omega0*t0)
                tmp0      = sin(arg0+sin0)
                rhot(i)   = rho0+rho1*tmp0
                t1        = real(i+1,kind=sp)
                arg1      = om0*t1
                sin1      = Beta*sin(Omega0*t1)
                tmp1      = sin(arg1+sin1)
                rhot(i+1) = rho0+rho1*tmp1
                t2        = real(i+2,kind=sp)
                arg2      = om0*t2
                sin2      = Beta*sin(Omega0*t2)
                tmp0      = sin(arg2+sin2)
                rhot(i+2) = rho0+rho1*tmp2
                t3        = real(i+3,kind=sp)
                arg3      = om0*t3
                sin3      = Beta*sin(Omega0*t3)
                tmp0      = sin(arg3+sin3)
                rhot(i+3) = rho0+rho1*tmp3
                t4        = real(i+4,kind=sp)
                arg4      = om0*t4
                sin4      = Beta*sin(Omega0*t4)
                tmp4      = sin(arg4+sin4)
                rhot(i+4) = rho0+rho1*tmp4
                t5        = real(i+5,kind=sp)
                arg5      = om0*t5
                sin5      = Beta*sin(Omega0*t5)
                tmp5      = sin(arg5+sin5)
                rhot(i+5) = rho0+rho1*tmp5
                t6        = real(i+6,kind=sp)
                arg6      = om0*t6
                sin5      = Beta*sin(Omega0*t6)
                tmp6      = sin(arg6+sin6)
                rhot(i+6) = rho0+rho1*tmp6
                t7        = real(i+7,kind=sp)
                arg7      = om0*t7
                sin7      = Beta*sin(Omega0*t7)
                tmp7      = sin(arg7+sin7)
                rhot(i+7) = rho0+rho1*tmp7
                t8        = real(i+8,kind=sp)
                arg8      = om0*t8
                sin8      = Beta*sin(Omega0*t8)
                tmp8      = sin(arg8+sin8)
                rhot(i+8) = rho0+rho1*tmp8
                t9        = real(i+9,kind=sp)
                arg9      = om0*t9
                sin9      = Beta*sin(Omega0*t9)
                tmp9      = sin(arg9+sin9)
                rhot(i+9) = rho0+rho1*tmp9
                t10       = real(i+10,kind=sp)
                arg10     = om0*t10
                sin10     = Beta*sin(Omega0*t10)
                tmp10     = sin(arg10+sin10)
                rhot(i+10)= rho0+rho1*tmp10
                t11       = real(i+11,kind=sp)
                arg11     = om0*t11
                sin11     = Beta*sin(Omega0*t11)
                tmp11     = sin(arg11+sin11)
                rhot(i+11)= rho0+rho1*tmp11
                t12       = real(i+12,kind=sp)
                arg12     = om0*t12
                sin12     = Beta*sin(Omega0*t12)
                tmp12     = sin(arg12+sin12)
                rhot(i+12)= rho0+rho1*tmp12
                t13       = real(i+13,kind=sp)
                arg13     = om0*t13
                sin13     = Beta*sin(Omega0*t13)
                tmp13     = sin(arg13+sin13)
                rhot(i+13)= rho0+rho1*tmp13
                t14       = real(i+14,kind=sp)
                arg14     = om0*t14
                sin14     = Beta*sin(Omega0*t14)
                tmp14     = sin(arg14+sin14)
                rhot(i+14)= rho0+rho1*tmp14
                t15       = real(i+15,kind=sp)
                arg15     = om0*t15
                sin15     = Beta*sin(Omega0*t15)
                tmp15     = sin(arg15+sin15)
                rhot(i+15)= rho0+rho1*tmp15
            end do
       end subroutine transmit_coeff_unroll_16x_r4


       subroutine transmit_coef_unroll_16x_r8(rho0,rho1,rhot,n,Beta,Omega0,om0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmit_coef_unroll_16x_r8
           !dir$ attributes forceinline ::   transmit_coef_unroll_16x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmit_coef_unroll_16x_r8
           real(kind=dp),                 intent(in)  :: rho0
           real(kind=dp),                 intent(in)  :: rho1
           real(kind=dp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp),                 intent(in)  :: Beta
           real(kind=dp),                 intent(in)  :: Omega0
           real(kind=dp),                 intent(in)  :: om0
           real(kind=dp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=dp), automatic :: t8,t9,t10,t11,t12,t13,t14,t15
           real(kind=dp), automatic :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           real(kind=dp), automatic :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           real(kind=dp), automatic :: sin0,sin1,sin2,sin3,sin4,sin5,sin6,sin7
           real(kind=dp), automatic :: sin8,sin9,sin10,sin11,sin12,sin13,sin14,sin15
           real(kind=dp), automatic :: tmp0,tmp1,tmp2,tmp3,tmp4,tmp,tmp6,tmp7
           real(kind=dp), automatic :: tmp8,tmp9,tmp10,tmp11,tmp12,tmp13,tmp14,tmp15
           integer(kind=i4) :: i,m,m1
           m = mod(n,16)
           if(m /= 0) then
              do i=1,m
                 t0      = real(i,kind=dp)
                 arg0    = om0*t0
                 sin0    = Beta*sin(Omega0*t0)
                 tmp0    = sin(arg0+sin0)
                 rhot(i) = rho0+rho1*tmp0
              end do
              if(n<16) return
            end if 
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,16
                t0        = real(i,kind=dp)
                arg0      = om0*t0
                sin0      = Beta*sin(Omega0*t0)
                tmp0      = sin(arg0+sin0)
                rhot(i)   = rho0+rho1*tmp0
                t1        = real(i+1,kind=dp)
                arg1      = om0*t1
                sin1      = Beta*sin(Omega0*t1)
                tmp1      = sin(arg1+sin1)
                rhot(i+1) = rho0+rho1*tmp1
                t2        = real(i+2,kind=dp)
                arg2      = om0*t2
                sin2      = Beta*sin(Omega0*t2)
                tmp0      = sin(arg2+sin2)
                rhot(i+2) = rho0+rho1*tmp2
                t3        = real(i+3,kind=dp)
                arg3      = om0*t3
                sin3      = Beta*sin(Omega0*t3)
                tmp0      = sin(arg3+sin3)
                rhot(i+3) = rho0+rho1*tmp3
                t4        = real(i+4,kind=dp)
                arg4      = om0*t4
                sin4      = Beta*sin(Omega0*t4)
                tmp4      = sin(arg4+sin4)
                rhot(i+4) = rho0+rho1*tmp4
                t5        = real(i+5,kind=dp)
                arg5      = om0*t5
                sin5      = Beta*sin(Omega0*t5)
                tmp5      = sin(arg5+sin5)
                rhot(i+5) = rho0+rho1*tmp5
                t6        = real(i+6,kind=dp)
                arg6      = om0*t6
                sin5      = Beta*sin(Omega0*t6)
                tmp6      = sin(arg6+sin6)
                rhot(i+6) = rho0+rho1*tmp6
                t7        = real(i+7,kind=dp)
                arg7      = om0*t7
                sin7      = Beta*sin(Omega0*t7)
                tmp7      = sin(arg7+sin7)
                rhot(i+7) = rho0+rho1*tmp7
                t8        = real(i+8,kind=dp)
                arg8      = om0*t8
                sin8      = Beta*sin(Omega0*t8)
                tmp8      = sin(arg8+sin8)
                rhot(i+8) = rho0+rho1*tmp8
                t9        = real(i+9,kind=dp)
                arg9      = om0*t9
                sin9      = Beta*sin(Omega0*t9)
                tmp9      = sin(arg9+sin9)
                rhot(i+9) = rho0+rho1*tmp9
                t10       = real(i+10,kind=dp)
                arg10     = om0*t10
                sin10     = Beta*sin(Omega0*t10)
                tmp10     = sin(arg10+sin10)
                rhot(i+10)= rho0+rho1*tmp10
                t11       = real(i+11,kind=dp)
                arg11     = om0*t11
                sin11     = Beta*sin(Omega0*t11)
                tmp11     = sin(arg11+sin11)
                rhot(i+11)= rho0+rho1*tmp11
                t12       = real(i+12,kind=dp)
                arg12     = om0*t12
                sin12     = Beta*sin(Omega0*t12)
                tmp12     = sin(arg12+sin12)
                rhot(i+12)= rho0+rho1*tmp12
                t13       = real(i+13,kind=dp)
                arg13     = om0*t13
                sin13     = Beta*sin(Omega0*t13)
                tmp13     = sin(arg13+sin13)
                rhot(i+13)= rho0+rho1*tmp13
                t14       = real(i+14,kind=dp)
                arg14     = om0*t14
                sin14     = Beta*sin(Omega0*t14)
                tmp14     = sin(arg14+sin14)
                rhot(i+14)= rho0+rho1*tmp14
                t15       = real(i+15,kind=dp)
                arg15     = om0*t15
                sin15     = Beta*sin(Omega0*t15)
                tmp15     = sin(arg15+sin15)
                rhot(i+15)= rho0+rho1*tmp15
            end do
       end subroutine transmit_coeff_unroll_16x_r8


       subroutine transmit_coef_unroll_8x_r4(rho0,rho1,rhot,n,Beta,Omega0,om0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmit_coef_unroll_8x_r4
           !dir$ attributes forceinline ::   transmit_coef_unroll_8x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmit_coef_unroll_8x_r4
           real(kind=sp),                 intent(in)  :: rho0
           real(kind=sp),                 intent(in)  :: rho1
           real(kind=sp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp),                 intent(in)  :: Beta
           real(kind=sp),                 intent(in)  :: Omega0
           real(kind=sp),                 intent(in)  :: om0
           real(kind=sp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=sp), automatic :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           real(kind=sp), automatic :: sin0,sin1,sin2,sin3,sin4,sin5,sin6,sin7
           real(kind=sp), automatic :: tmp0,tmp1,tmp2,tmp3,tmp4,tmp,tmp6,tmp7
           integer(kind=i4) :: i,m,m1
           m = mod(n,8)
           if(m /= 0) then
              do i=1,m
                 t0      = real(i,kind=sp)
                 arg0    = om0*t0
                 sin0    = Beta*sin(Omega0*t0)
                 tmp0    = sin(arg0+sin0)
                 rhot(i) = rho0+rho1*tmp0
              end do
              if(n<8) return
            end if 
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,8
                t0        = real(i,kind=sp)
                arg0      = om0*t0
                sin0      = Beta*sin(Omega0*t0)
                tmp0      = sin(arg0+sin0)
                rhot(i)   = rho0+rho1*tmp0
                t1        = real(i+1,kind=sp)
                arg1      = om0*t1
                sin1      = Beta*sin(Omega0*t1)
                tmp1      = sin(arg1+sin1)
                rhot(i+1) = rho0+rho1*tmp1
                t2        = real(i+2,kind=sp)
                arg2      = om0*t2
                sin2      = Beta*sin(Omega0*t2)
                tmp0      = sin(arg2+sin2)
                rhot(i+2) = rho0+rho1*tmp2
                t3        = real(i+3,kind=sp)
                arg3      = om0*t3
                sin3      = Beta*sin(Omega0*t3)
                tmp0      = sin(arg3+sin3)
                rhot(i+3) = rho0+rho1*tmp3
                t4        = real(i+4,kind=sp)
                arg4      = om0*t4
                sin4      = Beta*sin(Omega0*t4)
                tmp4      = sin(arg4+sin4)
                rhot(i+4) = rho0+rho1*tmp4
                t5        = real(i+5,kind=sp)
                arg5      = om0*t5
                sin5      = Beta*sin(Omega0*t5)
                tmp5      = sin(arg5+sin5)
                rhot(i+5) = rho0+rho1*tmp5
                t6        = real(i+6,kind=sp)
                arg6      = om0*t6
                sin5      = Beta*sin(Omega0*t6)
                tmp6      = sin(arg6+sin6)
                rhot(i+6) = rho0+rho1*tmp6
                t7        = real(i+7,kind=sp)
                arg7      = om0*t7
                sin7      = Beta*sin(Omega0*t7)
                tmp7      = sin(arg7+sin7)
                rhot(i+7) = rho0+rho1*tmp7
               
            end do
       end subroutine transmit_coeff_unroll_8x_r4


       subroutine transmit_coef_unroll_8x_r8(rho0,rho1,rhot,n,Beta,Omega0,om0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmit_coef_unroll_8x_r8
           !dir$ attributes forceinline ::   transmit_coef_unroll_8x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmit_coef_unroll_8x_r8
           real(kind=dp),                 intent(in)  :: rho0
           real(kind=dp),                 intent(in)  :: rho1
           real(kind=dp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp),                 intent(in)  :: Beta
           real(kind=dp),                 intent(in)  :: Omega0
           real(kind=dp),                 intent(in)  :: om0
           real(kind=dp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=dp), automatic :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           real(kind=dp), automatic :: sin0,sin1,sin2,sin3,sin4,sin5,sin6,sin7
           real(kind=dp), automatic :: tmp0,tmp1,tmp2,tmp3,tmp4,tmp,tmp6,tmp7
           integer(kind=i4) :: i,m,m1
           m = mod(n,8)
           if(m /= 0) then
              do i=1,m
                 t0      = real(i,kind=dp)
                 arg0    = om0*t0
                 sin0    = Beta*sin(Omega0*t0)
                 tmp0    = sin(arg0+sin0)
                 rhot(i) = rho0+rho1*tmp0
              end do
              if(n<8) return
            end if 
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,8
                t0        = real(i,kind=dp)
                arg0      = om0*t0
                sin0      = Beta*sin(Omega0*t0)
                tmp0      = sin(arg0+sin0)
                rhot(i)   = rho0+rho1*tmp0
                t1        = real(i+1,kind=dp)
                arg1      = om0*t1
                sin1      = Beta*sin(Omega0*t1)
                tmp1      = sin(arg1+sin1)
                rhot(i+1) = rho0+rho1*tmp1
                t2        = real(i+2,kind=dp)
                arg2      = om0*t2
                sin2      = Beta*sin(Omega0*t2)
                tmp0      = sin(arg2+sin2)
                rhot(i+2) = rho0+rho1*tmp2
                t3        = real(i+3,kind=dp)
                arg3      = om0*t3
                sin3      = Beta*sin(Omega0*t3)
                tmp0      = sin(arg3+sin3)
                rhot(i+3) = rho0+rho1*tmp3
                t4        = real(i+4,kind=dp)
                arg4      = om0*t4
                sin4      = Beta*sin(Omega0*t4)
                tmp4      = sin(arg4+sin4)
                rhot(i+4) = rho0+rho1*tmp4
                t5        = real(i+5,kind=dp)
                arg5      = om0*t5
                sin5      = Beta*sin(Omega0*t5)
                tmp5      = sin(arg5+sin5)
                rhot(i+5) = rho0+rho1*tmp5
                t6        = real(i+6,kind=dp)
                arg6      = om0*t6
                sin5      = Beta*sin(Omega0*t6)
                tmp6      = sin(arg6+sin6)
                rhot(i+6) = rho0+rho1*tmp6
                t7        = real(i+7,kind=dp)
                arg7      = om0*t7
                sin7      = Beta*sin(Omega0*t7)
                tmp7      = sin(arg7+sin7)
                rhot(i+7) = rho0+rho1*tmp7
               
            end do
       end subroutine transmit_coeff_unroll_8x_r8


       subroutine transmit_coef_unroll_4x_r4(rho0,rho1,rhot,n,Beta,Omega0,om0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmit_coef_unroll_4x_r4
           !dir$ attributes forceinline ::   transmit_coef_unroll_4x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmit_coef_unroll_4x_r4
           real(kind=sp),                 intent(in)  :: rho0
           real(kind=sp),                 intent(in)  :: rho1
           real(kind=sp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp),                 intent(in)  :: Beta
           real(kind=sp),                 intent(in)  :: Omega0
           real(kind=sp),                 intent(in)  :: om0
           real(kind=sp), automatic :: t0,t1,t2,t3
           real(kind=sp), automatic :: arg0,arg1,arg2,arg3
           real(kind=sp), automatic :: sin0,sin1,sin2,sin3
           real(kind=sp), automatic :: tmp0,tmp1,tmp2,tmp3
           integer(kind=i4) :: i,m,m1
           m = mod(n,4)
           if(m /= 0) then
              do i=1,m
                 t0      = real(i,kind=sp)
                 arg0    = om0*t0
                 sin0    = Beta*sin(Omega0*t0)
                 tmp0    = sin(arg0+sin0)
                 rhot(i) = rho0+rho1*tmp0
              end do
              if(n<4) return
            end if 
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,4
                t0        = real(i,kind=sp)
                arg0      = om0*t0
                sin0      = Beta*sin(Omega0*t0)
                tmp0      = sin(arg0+sin0)
                rhot(i)   = rho0+rho1*tmp0
                t1        = real(i+1,kind=sp)
                arg1      = om0*t1
                sin1      = Beta*sin(Omega0*t1)
                tmp1      = sin(arg1+sin1)
                rhot(i+1) = rho0+rho1*tmp1
                t2        = real(i+2,kind=sp)
                arg2      = om0*t2
                sin2      = Beta*sin(Omega0*t2)
                tmp0      = sin(arg2+sin2)
                rhot(i+2) = rho0+rho1*tmp2
                t3        = real(i+3,kind=sp)
                arg3      = om0*t3
                sin3      = Beta*sin(Omega0*t3)
                tmp0      = sin(arg3+sin3)
                rhot(i+3) = rho0+rho1*tmp3
                              
            end do
       end subroutine transmit_coeff_unroll_4x_r4


       subroutine transmit_coef_unroll_4x_r8(rho0,rho1,rhot,n,Beta,Omega0,om0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmit_coef_unroll_4x_r8
           !dir$ attributes forceinline ::   transmit_coef_unroll_4x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmit_coef_unroll_4x_r8
           real(kind=dp),                 intent(in)  :: rho0
           real(kind=dp),                 intent(in)  :: rho1
           real(kind=dp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp),                 intent(in)  :: Beta
           real(kind=dp),                 intent(in)  :: Omega0
           real(kind=dp),                 intent(in)  :: om0
           real(kind=dp), automatic :: t0,t1,t2,t3
           real(kind=dp), automatic :: arg0,arg1,arg2,arg3
           real(kind=dp), automatic :: sin0,sin1,sin2,sin3
           real(kind=dp), automatic :: tmp0,tmp1,tmp2,tmp3
           integer(kind=i4) :: i,m,m1
           m = mod(n,4)
           if(m /= 0) then
              do i=1,m
                 t0      = real(i,kind=dp)
                 arg0    = om0*t0
                 sin0    = Beta*sin(Omega0*t0)
                 tmp0    = sin(arg0+sin0)
                 rhot(i) = rho0+rho1*tmp0
              end do
              if(n<4) return
            end if 
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,4
                t0        = real(i,kind=dp)
                arg0      = om0*t0
                sin0      = Beta*sin(Omega0*t0)
                tmp0      = sin(arg0+sin0)
                rhot(i)   = rho0+rho1*tmp0
                t1        = real(i+1,kind=dp)
                arg1      = om0*t1
                sin1      = Beta*sin(Omega0*t1)
                tmp1      = sin(arg1+sin1)
                rhot(i+1) = rho0+rho1*tmp1
                t2        = real(i+2,kind=dp)
                arg2      = om0*t2
                sin2      = Beta*sin(Omega0*t2)
                tmp0      = sin(arg2+sin2)
                rhot(i+2) = rho0+rho1*tmp2
                t3        = real(i+3,kind=dp)
                arg3      = om0*t3
                sin3      = Beta*sin(Omega0*t3)
                tmp0      = sin(arg3+sin3)
                rhot(i+3) = rho0+rho1*tmp3
                          
            end do
       end subroutine transmit_coeff_unroll_4x_r8


       subroutine transmit_coef_unroll_2x_r4(rho0,rho1,rhot,n,Beta,Omega0,om0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmit_coef_unroll_2x_r4
           !dir$ attributes forceinline ::   transmit_coef_unroll_2x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmit_coef_unroll_2x_r4
           real(kind=sp),                 intent(in)  :: rho0
           real(kind=sp),                 intent(in)  :: rho1
           real(kind=sp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp),                 intent(in)  :: Beta
           real(kind=sp),                 intent(in)  :: Omega0
           real(kind=sp),                 intent(in)  :: om0
           real(kind=sp), automatic :: t0,t1
           real(kind=sp), automatic :: arg1,arg2
           real(kind=sp), automatic :: sin0,sin1
           real(kind=sp), automatic :: tmp0,tmp1
           integer(kind=i4) :: i,m,m1
           m = mod(n,2)
           if(m /= 0) then
              do i=1,m
                 t0      = real(i,kind=sp)
                 arg0    = om0*t0
                 sin0    = Beta*sin(Omega0*t0)
                 tmp0    = sin(arg0+sin0)
                 rhot(i) = rho0+rho1*tmp0
              end do
              if(n<2) return
            end if 
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,2
                t0        = real(i,kind=sp)
                arg0      = om0*t0
                sin0      = Beta*sin(Omega0*t0)
                tmp0      = sin(arg0+sin0)
                rhot(i)   = rho0+rho1*tmp0
                t1        = real(i+1,kind=sp)
                arg1      = om0*t1
                sin1      = Beta*sin(Omega0*t1)
                tmp1      = sin(arg1+sin1)
                rhot(i+1) = rho0+rho1*tmp1
                                            
            end do
       end subroutine transmit_coeff_unroll_2x_r4


       subroutine transmit_coef_unroll_2x_r8(rho0,rho1,rhot,n,Beta,Omega0,om0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmit_coef_unroll_2x_r8
           !dir$ attributes forceinline ::   transmit_coef_unroll_2x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmit_coef_unroll_2x_r8
           real(kind=dp),                 intent(in)  :: rho0
           real(kind=dp),                 intent(in)  :: rho1
           real(kind=dp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp),                 intent(in)  :: Beta
           real(kind=dp),                 intent(in)  :: Omega0
           real(kind=dp),                 intent(in)  :: om0
           real(kind=dp), automatic :: t0,t1
           real(kind=dp), automatic :: arg0,arg1
           real(kind=dp), automatic :: sin0,sin1
           real(kind=dp), automatic :: tmp0,tmp1
           integer(kind=i4) :: i,m,m1
           m = mod(n,2)
           if(m /= 0) then
              do i=1,m
                 t0      = real(i,kind=dp)
                 arg0    = om0*t0
                 sin0    = Beta*sin(Omega0*t0)
                 tmp0    = sin(arg0+sin0)
                 rhot(i) = rho0+rho1*tmp0
              end do
              if(n<2) return
            end if 
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,2
                t0        = real(i,kind=dp)
                arg0      = om0*t0
                sin0      = Beta*sin(Omega0*t0)
                tmp0      = sin(arg0+sin0)
                rhot(i)   = rho0+rho1*tmp0
                t1        = real(i+1,kind=dp)
                arg1      = om0*t1
                sin1      = Beta*sin(Omega0*t1)
                tmp1      = sin(arg1+sin1)
                rhot(i+1) = rho0+rho1*tmp1
              
                          
            end do
       end subroutine transmit_coeff_unroll_2x_r8


       subroutine transmit_coef_r4(rho0,rho1,rhot,n,Beta,Omega0,om0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmit_coef_r4
           !dir$ attributes forceinline ::   transmit_coef_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmit_coef_r4
           real(kind=sp),                 intent(in)  :: rho0
           real(kind=sp),                 intent(in)  :: rho1
           real(kind=sp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp),                 intent(in)  :: Beta
           real(kind=sp),                 intent(in)  :: Omega0
           real(kind=sp),                 intent(in)  :: om0
           real(kind=sp), automatic :: t0
           real(kind=sp), automatic :: arg1
           real(kind=sp), automatic :: sin0
           real(kind=sp), automatic :: tmp0
           integer(kind=i4) :: i
          
            !dir$ assume_aligned rhot:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=1,n
                t0        = real(i,kind=sp)
                arg0      = om0*t0
                sin0      = Beta*sin(Omega0*t0)
                tmp0      = sin(arg0+sin0)
                rhot(i)   = rho0+rho1*tmp0
                                                           
            end do
       end subroutine transmit_coeff_r4


       
       subroutine transmit_coef_r8(rho0,rho1,rhot,n,Beta,Omega0,om0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmit_coef_r8
           !dir$ attributes forceinline ::   transmit_coef_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmit_coef_r8
           real(kind=dp),                 intent(in)  :: rho0
           real(kind=dp),                 intent(in)  :: rho1
           real(kind=dp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp),                 intent(in)  :: Beta
           real(kind=dp),                 intent(in)  :: Omega0
           real(kind=dp),                 intent(in)  :: om0
           real(kind=dp), automatic :: t0
           real(kind=dp), automatic :: arg1
           real(kind=dp), automatic :: sin0
           real(kind=dp), automatic :: tmp0
           integer(kind=i4) :: i
          
            !dir$ assume_aligned rhot:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=1,n
                t0        = real(i,kind=dp)
                arg0      = om0*t0
                sin0      = Beta*sin(Omega0*t0)
                tmp0      = sin(arg0+sin0)
                rhot(i)   = rho0+rho1*tmp0
                                                           
            end do
       end subroutine transmit_coeff_r8


       subroutine transmit_coeff_exec_r4(rho0,rho1,rhot,n,Beta,Omega0,om0,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: transmit_coeff_exec_r4
           real(kind=sp),                 intent(in)  :: rho0
           real(kind=sp),                 intent(in)  :: rho1
           real(kind=sp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp),                 intent(in)  :: Beta
           real(kind=sp),                 intent(in)  :: Omega0
           real(kind=sp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: unroll_cnt
           select case (unroll_cnt)
               case (16)
                 call transmit_coeff_unroll_16x_r4(rho0,rho1,rhot,n,Beta,Omega0,om0)
               case (8)
                 call transmit_coeff_unroll_8x_r4(rho0,rho1,rhot,n,Beta,Omega0,om0)
               case (4)
                 call transmit_coeff_unroll_4x_r4(rho0,rho1,rhot,n,Beta,Omega0,om0)
               case (2)
                 call transmit_coeff_unroll_2x_r4(rho0,rho1,rhot,n,Beta,Omega0,om0)
               case (0)
                 call transmit_coeff_r4(rho0,rho1,rhot,n,Beta,Omega0,om0)
               case default
                 return
           end select
       end subroutine transmit_coeff_exec_r4


       subroutine transmit_coeff_exec_r8(rho0,rho1,rhot,n,Beta,Omega0,om0,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: transmit_coeff_exec_r8
           real(kind=dp),                 intent(in)  :: rho0
           real(kind=dp),                 intent(in)  :: rho1
           real(kind=dp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp),                 intent(in)  :: Beta
           real(kind=dp),                 intent(in)  :: Omega0
           real(kind=dp),                 intent(in)  :: om0
           integer(kind=i4),              intent(in)  :: unroll_cnt
           select case (unroll_cnt)
               case (16)
                 call transmit_coeff_unroll_16x_r8(rho0,rho1,rhot,n,Beta,Omega0,om0)
               case (8)
                 call transmit_coeff_unroll_8x_r8(rho0,rho1,rhot,n,Beta,Omega0,om0)
               case (4)
                 call transmit_coeff_unroll_4x_r8(rho0,rho1,rhot,n,Beta,Omega0,om0)
               case (2)
                 call transmit_coeff_unroll_2x_r8(rho0,rho1,rhot,n,Beta,Omega0,om0)
               case (0)
                 call transmit_coeff_r8(rho0,rho1,rhot,n,Beta,Omega0,om0)
               case default
                 return
           end select
       end subroutine transmit_coeff_exec_r8




       !Formula 8, p. 192
       subroutine integrate_Ft_r4(Ft,absc,n,xup,np,gamt,ier)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  integrate_Ft_r4
            !dir$ attributes forceinline ::   integrate_Ft_r4
            use quadpack, only : savint
            use omp_lib
            real(kind=sp), dimension(1:n), intent(in)  :: Ft
            real(kind=sp), dimension(1:n), intent(in)  :: absc
            integer(kind=i4),              intent(in)  :: n
            real(kind=sp),                 intent(in)  :: xup
            real(kind=sp),                 intent(in)  :: np
            real(kind=sp),                 intent(out) :: gamt
            integer(kind=i4),              intent(out) :: ier
            real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
            real(kind=sp) :: fac
            integer(kind=i4) :: i4
            fac = twopi*np
            !dir$ assume_aligned Ft:64
            !$omp simd simdlen(4) reduction(*:Ft)
            do i=1,n
               Ft(i) = Ft(i)*fac
            end do
            call savint(Ft,absc,n,0.0_sp,xup,gamt,ier)
        end subroutine integrate_Ft_r4


        subroutine integrate_Ft_r8(Ft,absc,n,xup,np,gamt,ier)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  integrate_Ft_r8
            !dir$ attributes forceinline ::   integrate_Ft_r8
            use quadpack, only : davint
            use omp_lib
            real(kind=dp), dimension(1:n), intent(in)  :: Ft
            real(kind=dp), dimension(1:n), intent(in)  :: absc
            integer(kind=i4),              intent(in)  :: n
            real(kind=dp),                 intent(in)  :: xup
            real(kind=dp),                 intent(in)  :: np
            real(kind=dp),                 intent(out) :: gamt
            integer(kind=i4),              intent(out) :: ier
            real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
            real(kind=dp) :: fac
            integer(kind=i4) :: i4
            fac = twopi*np
            !dir$ assume_aligned Ft:64
            !$omp simd simdlen(8) reduction(*:Ft)
            do i=1,n
               Ft(i) = Ft(i)*fac
            end do
            call davint(Ft,absc,n,0.0_dp,xup,gamt,ier)
        end subroutine integrate_Ft_r8


        subroutine raster_transmittance_unroll_16x_r4(a,r,rho0,Omega0,Beta,R0,om0,rhot,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_transmittance_unroll_16x_r4
           !dir$ attributes forceinline ::   raster_transmittance_unroll_16x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_transmittance_unroll_16x_r4
           real(kind=sp),                 intent(in)  :: a
           real(kind=sp),                 intent(in)  :: r
           real(kind=sp),                 intent(in)  :: rho0
           real(kind=sp),                 intent(in)  :: Omega0
           real(kind=sp),                 intent(in)  :: Beta
           real(kind=sp),                 intent(in)  :: R0
           real(kind=sp),                 intent(in)  :: om0
           real(kind=sp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp), parameter :: c0 = 0.63661977236758134307553505349_sp
           real(kind=sp), automatic :: rho00,rho01,rho02,rho03,rho04,rho05,rho06,rho07
           real(kind=sp), automatic :: rho08,rho09,rho010,rho011,rho012,rho013,rho014,rho015
           real(kind=sp), automatic :: th0,th1,th2,th3,th4,th5,th6,th7
           real(kind=sp), automatic :: th8,th9,th10,th11,th12,th13,th14,th15
           real(kind=sp), automatic :: cos0,cos1,cos2,cos3,cos4,cos5,cos6,cos7
           real(kind=sp), automatic :: cos8,cos9,cos10,cos11,cos12,cos13,cos14,cos15
           real(kind=sp), automatic :: sin0,sin1,sin2,sin3,sin4,sin5,sin6,sin7
           real(kind=sp), automatic :: sin8,sin9,sin10,sin11,sin12,sin13,sin14,sin15
           real(kind=sp), automatic :: omt0,omt1,omt2,omt3,omt4,omt5,omt6,omt7
           real(kind=sp), automatic :: omt8,omt9,omt10,omt11,omt12,omt13,omt14,omt15
           real(kind=sp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15
           real(kind=sp), automatic :: gamt0,gamt1,gamt2,gamt3,gamt4,gamt5,gamt6,gamt7
           real(kind=sp), automatic :: gamt8,gamt9,gamt10,gamt11,gamt12,gamt13,gamt14,gamt15
           real(kind=sp) :: M
           integer(kind=i4) :: i,m,m1
           M = c0*(a/(r+r))
           m = mod(n,16)
           if(m /= 0) then
              do i = 1,m
                 t0      = real(i,kind=sp)
                 th0     = -abs(cos(Omega0*t0))
                 rh00    = rho0*(1.0_sp+M*th0)
                 omt0    = om0*t0
                 gamt0   = sin(Omega0*t0)
                 sin0    = sin(omt0+Beta*gamt0)
                 rhot(i) = sin0
               end do
               if(n<16) return
            end if
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,16
                 t0        = real(i,kind=sp)
                 th0       = -abs(cos(Omega0*t0))
                 rh00      = rho0*(1.0_sp+M*th0)
                 omt0      = om0*t0
                 gamt0     = sin(Omega0*t0)
                 sin0      = sin(omt0+Beta*gamt0)
                 rhot(i)   = sin0
                 t1        = real(i+1,kind=sp)
                 th1       = -abs(cos(Omega0*t1))
                 rh01      = rho0*(1.0_sp+M*th1)
                 omt1      = om0*t1
                 gamt1     = sin(Omega0*t1)
                 sin1      = sin(omt1+Beta*gamt1)
                 rhot(i+1) = sin1
                 t2        = real(i+2,kind=sp)
                 th2       = -abs(cos(Omega0*t2))
                 rh02      = rho0*(1.0_sp+M*th2)
                 omt2      = om0*t2
                 gamt2     = sin(Omega0*t2)
                 sin2      = sin(omt2+Beta*gamt2)
                 rhot(i+2) = sin2
                 t3        = real(i+3,kind=sp)
                 th3       = -abs(cos(Omega0*t3))
                 rh03      = rho0*(1.0_sp+M*th0)
                 omt3      = om0*t3
                 gamt3     = sin(Omega0*t3)
                 sin3      = sin(omt3+Beta*gamt3)
                 rhot(i+3) = sin3
                 t4        = real(i+4,kind=sp)
                 th4       = -abs(cos(Omega0*t4))
                 rh04      = rho0*(1.0_sp+M*th4)
                 omt4      = om0*t4
                 gamt4     = sin(Omega0*t4)
                 sin4      = sin(omt4+Beta*gamt4)
                 rhot(i+4) = sin4
                 t5        = real(i+5,kind=sp)
                 th5       = -abs(cos(Omega0*t5))
                 rh05      = rho0*(1.0_sp+M*th5)
                 omt5      = om0*t5
                 gamt5     = sin(Omega0*t5)
                 sin5      = sin(omt5+Beta*gamt5)
                 rhot(i+5) = sin5
                 t6        = real(i+6,kind=sp)
                 th6       = -abs(cos(Omega0*t6))
                 rh06      = rho0*(1.0_sp+M*th6)
                 omt6      = om0*t6
                 gamt6     = sin(Omega0*t6)
                 sin6      = sin(omt6+Beta*gamt6)
                 rhot(i+6) = sin6
                 t7        = real(i+7,kind=sp)
                 th7       = -abs(cos(Omega0*t7))
                 rh07      = rho0*(1.0_sp+M*th7)
                 omt7      = om0*t7
                 gamt7     = sin(Omega0*t7)
                 sin7      = sin(omt7+Beta*gamt7)
                 rhot(i+7)   = sin7
                 t8        = real(i+8,kind=sp)
                 th8       = -abs(cos(Omega0*t8))
                 rh08      = rho0*(1.0_sp+M*th8)
                 omt8      = om0*t8
                 gamt8     = sin(Omega0*t8)
                 sin8      = sin(omt8+Beta*gamt8)
                 rhot(i+8) = sin8
                 t9        = real(i+9,kind=sp)
                 th9       = -abs(cos(Omega0*t9))
                 rh09      = rho0*(1.0_sp+M*th9)
                 omt9      = om0*t9
                 gamt9     = sin(Omega0*t9)
                 sin9      = sin(omt9+Beta*gamt9)
                 rhot(i+9) = sin9
                 t10       = real(i+10,kind=sp)
                 th10      = -abs(cos(Omega0*t10))
                 rh010     = rho0*(1.0_sp+M*th10)
                 omt10     = om0*t10
                 gamt10    = sin(Omega0*t10)
                 sin10     = sin(omt10+Beta*gamt10)
                 rhot(i+10)= sin10 
                 t11       = real(i+11,kind=sp)
                 th11      = -abs(cos(Omega0*t11))
                 rh011     = rho0*(1.0_sp+M*th11)
                 omt11     = om0*t11
                 gamt11    = sin(Omega0*t11)
                 sin11     = sin(omt11+Beta*gamt11)
                 rhot(i+11)= sin11
                 t12       = real(i+12,kind=sp)
                 th12      = -abs(cos(Omega0*t12))
                 rh012     = rho0*(1.0_sp+M*th12)
                 omt12     = om0*t12
                 gamt12    = sin(Omega0*t12)
                 sin12     = sin(omt12+Beta*gamt12)
                 rhot(i+12)= sin12
                 t13       = real(i+13,kind=sp)
                 th13      = -abs(cos(Omega0*t13))
                 rh013     = rho0*(1.0_sp+M*th13)
                 omt13     = om0*t13
                 gamt13    = sin(Omega0*t13)
                 sin13     = sin(omt13+Beta*gamt13)
                 rhot(i+13)= sin13
                 t14       = real(i+14,kind=sp)
                 th14      = -abs(cos(Omega0*t14))
                 rh014     = rho0*(1.0_sp+M*th14)
                 omt14     = om0*t14
                 gamt14    = sin(Omega0*t14)
                 sin14     = sin(omt14+Beta*gamt14)
                 rhot(i+14)= sin14
                 t15       = real(i+15,kind=sp)
                 th15      = -abs(cos(Omega0*t15))
                 rh015     = rho0*(1.0_sp+M*th15)
                 omt15     = om0*t15
                 gamt15    = sin(Omega0*t15)
                 sin15     = sin(omt15+Beta*gamt15)
                 rhot(i+15)= sin15
            end do
       end subroutine raster_transmittance_unroll_16x_r4


      subroutine raster_transmittance_unroll_16x_r8(a,r,rho0,Omega0,Beta,R0,om0,rhot,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_transmittance_unroll_16x_r8
           !dir$ attributes forceinline ::   raster_transmittance_unroll_16x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_transmittance_unroll_16x_r8
           real(kind=dp),                 intent(in)  :: a
           real(kind=dp),                 intent(in)  :: r
           real(kind=dp),                 intent(in)  :: rho0
           real(kind=dp),                 intent(in)  :: Omega0
           real(kind=dp),                 intent(in)  :: Beta
           real(kind=dp),                 intent(in)  :: R0
           real(kind=dp),                 intent(in)  :: om0
           real(kind=dp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp), parameter :: c0 = 0.63661977236758134307553505349_dp
           real(kind=dp), automatic :: rho00,rho01,rho02,rho03,rho04,rho05,rho06,rho07
           real(kind=dp), automatic :: rho08,rho09,rho010,rho011,rho012,rho013,rho014,rho015
           real(kind=dp), automatic :: th0,th1,th2,th3,th4,th5,th6,th7
           real(kind=dp), automatic :: th8,th9,th10,th11,th12,th13,th14,th15
           real(kind=dp), automatic :: cos0,cos1,cos2,cos3,cos4,cos5,cos6,cos7
           real(kind=dp), automatic :: cos8,cos9,cos10,cos11,cos12,cos13,cos14,cos15
           real(kind=dp), automatic :: sin0,sin1,sin2,sin3,sin4,sin5,sin6,sin7
           real(kind=dp), automatic :: sin8,sin9,sin10,sin11,sin12,sin13,sin14,sin15
           real(kind=dp), automatic :: omt0,omt1,omt2,omt3,omt4,omt5,omt6,omt7
           real(kind=dp), automatic :: omt8,omt9,omt10,omt11,omt12,omt13,omt14,omt15
           real(kind=dp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15
           real(kind=dp), automatic :: gamt0,gamt1,gamt2,gamt3,gamt4,gamt5,gamt6,gamt7
           real(kind=dp), automatic :: gamt8,gamt9,gamt10,gamt11,gamt12,gamt13,gamt14,gamt15
           real(kind=dp) :: M
           integer(kind=i4) :: i,m,m1
           M = c0*(a/(r+r))
           m = mod(n,16)
           if(m /= 0) then
              do i = 1,m
                 t0      = real(i,kind=dp)
                 th0     = -abs(cos(Omega0*t0))
                 rh00    = rho0*(1.0_sp+M*th0)
                 omt0    = om0*t0
                 gamt0   = sin(Omega0*t0)
                 sin0    = sin(omt0+Beta*gamt0)
                 rhot(i) = sin0
               end do
               if(n<16) return
            end if
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,16
                 t0        = real(i,kind=dp)
                 th0       = -abs(cos(Omega0*t0))
                 rh00      = rho0*(1.0_dp+M*th0)
                 omt0      = om0*t0
                 gamt0     = sin(Omega0*t0)
                 sin0      = sin(omt0+Beta*gamt0)
                 rhot(i)   = sin0
                 t1        = real(i+1,kind=dp)
                 th1       = -abs(cos(Omega0*t1))
                 rh01      = rho0*(1.0_dp+M*th1)
                 omt1      = om0*t1
                 gamt1     = sin(Omega0*t1)
                 sin1      = sin(omt1+Beta*gamt1)
                 rhot(i+1) = sin1
                 t2        = real(i+2,kind=dp)
                 th2       = -abs(cos(Omega0*t2))
                 rh02      = rho0*(1.0_dp+M*th2)
                 omt2      = om0*t2
                 gamt2     = sin(Omega0*t2)
                 sin2      = sin(omt2+Beta*gamt2)
                 rhot(i+2) = sin2
                 t3        = real(i+3,kind=dp)
                 th3       = -abs(cos(Omega0*t3))
                 rh03      = rho0*(1.0_dp+M*th0)
                 omt3      = om0*t3
                 gamt3     = sin(Omega0*t3)
                 sin3      = sin(omt3+Beta*gamt3)
                 rhot(i+3) = sin3
                 t4        = real(i+4,kind=dp)
                 th4       = -abs(cos(Omega0*t4))
                 rh04      = rho0*(1.0_dp+M*th4)
                 omt4      = om0*t4
                 gamt4     = sin(Omega0*t4)
                 sin4      = sin(omt4+Beta*gamt4)
                 rhot(i+4) = sin4
                 t5        = real(i+5,kind=dp)
                 th5       = -abs(cos(Omega0*t5))
                 rh05      = rho0*(1.0_dp+M*th5)
                 omt5      = om0*t5
                 gamt5     = sin(Omega0*t5)
                 sin5      = sin(omt5+Beta*gamt5)
                 rhot(i+5) = sin5
                 t6        = real(i+6,kind=dp)
                 th6       = -abs(cos(Omega0*t6))
                 rh06      = rho0*(1.0_dp+M*th6)
                 omt6      = om0*t6
                 gamt6     = sin(Omega0*t6)
                 sin6      = sin(omt6+Beta*gamt6)
                 rhot(i+6) = sin6
                 t7        = real(i+7,kind=dp)
                 th7       = -abs(cos(Omega0*t7))
                 rh07      = rho0*(1.0_dp+M*th7)
                 omt7      = om0*t7
                 gamt7     = sin(Omega0*t7)
                 sin7      = sin(omt7+Beta*gamt7)
                 rhot(i+7)   = sin7
                 t8        = real(i+8,kind=dp)
                 th8       = -abs(cos(Omega0*t8))
                 rh08      = rho0*(1.0_dp+M*th8)
                 omt8      = om0*t8
                 gamt8     = sin(Omega0*t8)
                 sin8      = sin(omt8+Beta*gamt8)
                 rhot(i+8) = sin8
                 t9        = real(i+9,kind=dp)
                 th9       = -abs(cos(Omega0*t9))
                 rh09      = rho0*(1.0_dp+M*th9)
                 omt9      = om0*t9
                 gamt9     = sin(Omega0*t9)
                 sin9      = sin(omt9+Beta*gamt9)
                 rhot(i+9) = sin9
                 t10       = real(i+10,kind=dp)
                 th10      = -abs(cos(Omega0*t10))
                 rh010     = rho0*(1.0_dp+M*th10)
                 omt10     = om0*t10
                 gamt10    = sin(Omega0*t10)
                 sin10     = sin(omt10+Beta*gamt10)
                 rhot(i+10)= sin10 
                 t11       = real(i+11,kind=dp)
                 th11      = -abs(cos(Omega0*t11))
                 rh011     = rho0*(1.0_dp+M*th11)
                 omt11     = om0*t11
                 gamt11    = sin(Omega0*t11)
                 sin11     = sin(omt11+Beta*gamt11)
                 rhot(i+11)= sin11
                 t12       = real(i+12,kind=dp)
                 th12      = -abs(cos(Omega0*t12))
                 rh012     = rho0*(1.0_dp+M*th12)
                 omt12     = om0*t12
                 gamt12    = sin(Omega0*t12)
                 sin12     = sin(omt12+Beta*gamt12)
                 rhot(i+12)= sin12
                 t13       = real(i+13,kind=dp)
                 th13      = -abs(cos(Omega0*t13))
                 rh013     = rho0*(1.0_dp+M*th13)
                 omt13     = om0*t13
                 gamt13    = sin(Omega0*t13)
                 sin13     = sin(omt13+Beta*gamt13)
                 rhot(i+13)= sin13
                 t14       = real(i+14,kind=dp)
                 th14      = -abs(cos(Omega0*t14))
                 rh014     = rho0*(1.0_dp+M*th14)
                 omt14     = om0*t14
                 gamt14    = sin(Omega0*t14)
                 sin14     = sin(omt14+Beta*gamt14)
                 rhot(i+14)= sin14
                 t15       = real(i+15,kind=dp)
                 th15      = -abs(cos(Omega0*t15))
                 rh015     = rho0*(1.0_dp+M*th15)
                 omt15     = om0*t15
                 gamt15    = sin(Omega0*t15)
                 sin15     = sin(omt15+Beta*gamt15)
                 rhot(i+15)= sin15
            end do
       end subroutine raster_transmittance_unroll_16x_r8


       subroutine raster_transmittance_unroll_8x_r4(a,r,rho0,Omega0,Beta,R0,om0,rhot,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_transmittance_unroll_8x_r4
           !dir$ attributes forceinline ::   raster_transmittance_unroll_8x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_transmittance_unroll_8x_r4
           real(kind=sp),                 intent(in)  :: a
           real(kind=sp),                 intent(in)  :: r
           real(kind=sp),                 intent(in)  :: rho0
           real(kind=sp),                 intent(in)  :: Omega0
           real(kind=sp),                 intent(in)  :: Beta
           real(kind=sp),                 intent(in)  :: R0
           real(kind=sp),                 intent(in)  :: om0
           real(kind=sp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp), parameter :: c0 = 0.63661977236758134307553505349_sp
           real(kind=sp), automatic :: rho00,rho01,rho02,rho03,rho04,rho05,rho06,rho07
           real(kind=sp), automatic :: th0,th1,th2,th3,th4,th5,th6,th7
           real(kind=sp), automatic :: cos0,cos1,cos2,cos3,cos4,cos5,cos6,cos7
           real(kind=sp), automatic :: sin0,sin1,sin2,sin3,sin4,sin5,sin6,sin7
           real(kind=sp), automatic :: omt0,omt1,omt2,omt3,omt4,omt5,omt6,omt7
           real(kind=sp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=sp), automatic :: gamt0,gamt1,gamt2,gamt3,gamt4,gamt5,gamt6,gamt7
           real(kind=sp) :: M
           integer(kind=i4) :: i,m,m1
           M = c0*(a/(r+r))
           m = mod(n,8)
           if(m /= 0) then
              do i = 1,m
                 t0      = real(i,kind=sp)
                 th0     = -abs(cos(Omega0*t0))
                 rh00    = rho0*(1.0_sp+M*th0)
                 omt0    = om0*t0
                 gamt0   = sin(Omega0*t0)
                 sin0    = sin(omt0+Beta*gamt0)
                 rhot(i) = sin0
               end do
               if(n<8) return
            end if
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,8
                 t0        = real(i,kind=sp)
                 th0       = -abs(cos(Omega0*t0))
                 rh00      = rho0*(1.0_sp+M*th0)
                 omt0      = om0*t0
                 gamt0     = sin(Omega0*t0)
                 sin0      = sin(omt0+Beta*gamt0)
                 rhot(i)   = sin0
                 t1        = real(i+1,kind=sp)
                 th1       = -abs(cos(Omega0*t1))
                 rh01      = rho0*(1.0_sp+M*th1)
                 omt1      = om0*t1
                 gamt1     = sin(Omega0*t1)
                 sin1      = sin(omt1+Beta*gamt1)
                 rhot(i+1) = sin1
                 t2        = real(i+2,kind=sp)
                 th2       = -abs(cos(Omega0*t2))
                 rh02      = rho0*(1.0_sp+M*th2)
                 omt2      = om0*t2
                 gamt2     = sin(Omega0*t2)
                 sin2      = sin(omt2+Beta*gamt2)
                 rhot(i+2) = sin2
                 t3        = real(i+3,kind=sp)
                 th3       = -abs(cos(Omega0*t3))
                 rh03      = rho0*(1.0_sp+M*th0)
                 omt3      = om0*t3
                 gamt3     = sin(Omega0*t3)
                 sin3      = sin(omt3+Beta*gamt3)
                 rhot(i+3) = sin3
                 t4        = real(i+4,kind=sp)
                 th4       = -abs(cos(Omega0*t4))
                 rh04      = rho0*(1.0_sp+M*th4)
                 omt4      = om0*t4
                 gamt4     = sin(Omega0*t4)
                 sin4      = sin(omt4+Beta*gamt4)
                 rhot(i+4) = sin4
                 t5        = real(i+5,kind=sp)
                 th5       = -abs(cos(Omega0*t5))
                 rh05      = rho0*(1.0_sp+M*th5)
                 omt5      = om0*t5
                 gamt5     = sin(Omega0*t5)
                 sin5      = sin(omt5+Beta*gamt5)
                 rhot(i+5) = sin5
                 t6        = real(i+6,kind=sp)
                 th6       = -abs(cos(Omega0*t6))
                 rh06      = rho0*(1.0_sp+M*th6)
                 omt6      = om0*t6
                 gamt6     = sin(Omega0*t6)
                 sin6      = sin(omt6+Beta*gamt6)
                 rhot(i+6) = sin6
                 t7        = real(i+7,kind=sp)
                 th7       = -abs(cos(Omega0*t7))
                 rh07      = rho0*(1.0_sp+M*th7)
                 omt7      = om0*t7
                 gamt7     = sin(Omega0*t7)
                 sin7      = sin(omt7+Beta*gamt7)
                 rhot(i+7) = sin7
                
            end do
       end subroutine raster_transmittance_unroll_8x_r4


       subroutine raster_transmittance_unroll_8x_r8(a,r,rho0,Omega0,Beta,R0,om0,rhot,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_transmittance_unroll_8x_r8
           !dir$ attributes forceinline ::   raster_transmittance_unroll_8x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_transmittance_unroll_8x_r8
           real(kind=dp),                 intent(in)  :: a
           real(kind=dp),                 intent(in)  :: r
           real(kind=dp),                 intent(in)  :: rho0
           real(kind=dp),                 intent(in)  :: Omega0
           real(kind=dp),                 intent(in)  :: Beta
           real(kind=dp),                 intent(in)  :: R0
           real(kind=dp),                 intent(in)  :: om0
           real(kind=dp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp), parameter :: c0 = 0.63661977236758134307553505349_dp
           real(kind=dp), automatic :: rho00,rho01,rho02,rho03,rho04,rho05,rho06,rho07
           real(kind=dp), automatic :: th0,th1,th2,th3,th4,th5,th6,th7
           real(kind=dp), automatic :: cos0,cos1,cos2,cos3,cos4,cos5,cos6,cos7
           real(kind=dp), automatic :: sin0,sin1,sin2,sin3,sin4,sin5,sin6,sin7
           real(kind=dp), automatic :: omt0,omt1,omt2,omt3,omt4,omt5,omt6,omt7
           real(kind=dp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=dp), automatic :: gamt0,gamt1,gamt2,gamt3,gamt4,gamt5,gamt6,gamt7
           real(kind=dp) :: M
           integer(kind=i4) :: i,m,m1
           M = c0*(a/(r+r))
           m = mod(n,8)
           if(m /= 0) then
              do i = 1,m
                 t0      = real(i,kind=dp)
                 th0     = -abs(cos(Omega0*t0))
                 rh00    = rho0*(1.0_sp+M*th0)
                 omt0    = om0*t0
                 gamt0   = sin(Omega0*t0)
                 sin0    = sin(omt0+Beta*gamt0)
                 rhot(i) = sin0
               end do
               if(n<8) return
            end if
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,8
                 t0        = real(i,kind=dp)
                 th0       = -abs(cos(Omega0*t0))
                 rh00      = rho0*(1.0_dp+M*th0)
                 omt0      = om0*t0
                 gamt0     = sin(Omega0*t0)
                 sin0      = sin(omt0+Beta*gamt0)
                 rhot(i)   = sin0
                 t1        = real(i+1,kind=dp)
                 th1       = -abs(cos(Omega0*t1))
                 rh01      = rho0*(1.0_dp+M*th1)
                 omt1      = om0*t1
                 gamt1     = sin(Omega0*t1)
                 sin1      = sin(omt1+Beta*gamt1)
                 rhot(i+1) = sin1
                 t2        = real(i+2,kind=dp)
                 th2       = -abs(cos(Omega0*t2))
                 rh02      = rho0*(1.0_dp+M*th2)
                 omt2      = om0*t2
                 gamt2     = sin(Omega0*t2)
                 sin2      = sin(omt2+Beta*gamt2)
                 rhot(i+2) = sin2
                 t3        = real(i+3,kind=dp)
                 th3       = -abs(cos(Omega0*t3))
                 rh03      = rho0*(1.0_dp+M*th0)
                 omt3      = om0*t3
                 gamt3     = sin(Omega0*t3)
                 sin3      = sin(omt3+Beta*gamt3)
                 rhot(i+3) = sin3
                 t4        = real(i+4,kind=dp)
                 th4       = -abs(cos(Omega0*t4))
                 rh04      = rho0*(1.0_dp+M*th4)
                 omt4      = om0*t4
                 gamt4     = sin(Omega0*t4)
                 sin4      = sin(omt4+Beta*gamt4)
                 rhot(i+4) = sin4
                 t5        = real(i+5,kind=dp)
                 th5       = -abs(cos(Omega0*t5))
                 rh05      = rho0*(1.0_dp+M*th5)
                 omt5      = om0*t5
                 gamt5     = sin(Omega0*t5)
                 sin5      = sin(omt5+Beta*gamt5)
                 rhot(i+5) = sin5
                 t6        = real(i+6,kind=dp)
                 th6       = -abs(cos(Omega0*t6))
                 rh06      = rho0*(1.0_dp+M*th6)
                 omt6      = om0*t6
                 gamt6     = sin(Omega0*t6)
                 sin6      = sin(omt6+Beta*gamt6)
                 rhot(i+6) = sin6
                 t7        = real(i+7,kind=dp)
                 th7       = -abs(cos(Omega0*t7))
                 rh07      = rho0*(1.0_dp+M*th7)
                 omt7      = om0*t7
                 gamt7     = sin(Omega0*t7)
                 sin7      = sin(omt7+Beta*gamt7)
                 rhot(i+7)   = sin7
               
            end do
       end subroutine raster_transmittance_unroll_8x_r8


       subroutine raster_transmittance_unroll_4x_r4(a,r,rho0,Omega0,Beta,R0,om0,rhot,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_transmittance_unroll_4x_r4
           !dir$ attributes forceinline ::   raster_transmittance_unroll_4x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_transmittance_unroll_4x_r4
           real(kind=sp),                 intent(in)  :: a
           real(kind=sp),                 intent(in)  :: r
           real(kind=sp),                 intent(in)  :: rho0
           real(kind=sp),                 intent(in)  :: Omega0
           real(kind=sp),                 intent(in)  :: Beta
           real(kind=sp),                 intent(in)  :: R0
           real(kind=sp),                 intent(in)  :: om0
           real(kind=sp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp), parameter :: c0 = 0.63661977236758134307553505349_sp
           real(kind=sp), automatic :: rho00,rho01,rho02,rho03
           real(kind=sp), automatic :: th0,th1,th2,th3
           real(kind=sp), automatic :: cos0,cos1,cos2,cos3
           real(kind=sp), automatic :: sin0,sin1,sin2,sin3
           real(kind=sp), automatic :: omt0,omt1,omt2,omt3
           real(kind=sp), automatic :: t0,t1,t2,t3
           real(kind=sp), automatic :: gamt0,gamt1,gamt2,gamt3
           real(kind=sp) :: M
           integer(kind=i4) :: i,m,m1
           M = c0*(a/(r+r))
           m = mod(n,4)
           if(m /= 0) then
              do i = 1,m
                 t0      = real(i,kind=sp)
                 th0     = -abs(cos(Omega0*t0))
                 rh00    = rho0*(1.0_sp+M*th0)
                 omt0    = om0*t0
                 gamt0   = sin(Omega0*t0)
                 sin0    = sin(omt0+Beta*gamt0)
                 rhot(i) = sin0
               end do
               if(n<4) return
            end if
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,4
                 t0        = real(i,kind=sp)
                 th0       = -abs(cos(Omega0*t0))
                 rh00      = rho0*(1.0_sp+M*th0)
                 omt0      = om0*t0
                 gamt0     = sin(Omega0*t0)
                 sin0      = sin(omt0+Beta*gamt0)
                 rhot(i)   = sin0
                 t1        = real(i+1,kind=sp)
                 th1       = -abs(cos(Omega0*t1))
                 rh01      = rho0*(1.0_sp+M*th1)
                 omt1      = om0*t1
                 gamt1     = sin(Omega0*t1)
                 sin1      = sin(omt1+Beta*gamt1)
                 rhot(i+1) = sin1
                 t2        = real(i+2,kind=sp)
                 th2       = -abs(cos(Omega0*t2))
                 rh02      = rho0*(1.0_sp+M*th2)
                 omt2      = om0*t2
                 gamt2     = sin(Omega0*t2)
                 sin2      = sin(omt2+Beta*gamt2)
                 rhot(i+2) = sin2
                 t3        = real(i+3,kind=sp)
                 th3       = -abs(cos(Omega0*t3))
                 rh03      = rho0*(1.0_sp+M*th0)
                 omt3      = om0*t3
                 gamt3     = sin(Omega0*t3)
                 sin3      = sin(omt3+Beta*gamt3)
                 rhot(i+3) = sin3
             
            end do
       end subroutine raster_transmittance_unroll_4x_r4


       subroutine raster_transmittance_unroll_4x_r8(a,r,rho0,Omega0,Beta,R0,om0,rhot,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_transmittance_unroll_4x_r8
           !dir$ attributes forceinline ::   raster_transmittance_unroll_4x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_transmittance_unroll_4x_r8
           real(kind=dp),                 intent(in)  :: a
           real(kind=dp),                 intent(in)  :: r
           real(kind=dp),                 intent(in)  :: rho0
           real(kind=dp),                 intent(in)  :: Omega0
           real(kind=dp),                 intent(in)  :: Beta
           real(kind=dp),                 intent(in)  :: R0
           real(kind=dp),                 intent(in)  :: om0
           real(kind=dp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp), parameter :: c0 = 0.63661977236758134307553505349_dp
           real(kind=dp), automatic :: rho00,rho01,rho02,rho03
           real(kind=dp), automatic :: th0,th1,th2,th3
           real(kind=dp), automatic :: cos0,cos1,cos2,cos3
           real(kind=dp), automatic :: sin0,sin1,sin2,sin3
           real(kind=dp), automatic :: omt0,omt1,omt2,omt3
           real(kind=dp), automatic :: t0,t1,t2,t3
           real(kind=dp), automatic :: gamt0,gamt1,gamt2,gamt3
           real(kind=dp) :: M
           integer(kind=i4) :: i,m,m1
           M = c0*(a/(r+r))
           m = mod(n,4)
           if(m /= 0) then
              do i = 1,m
                 t0      = real(i,kind=dp)
                 th0     = -abs(cos(Omega0*t0))
                 rh00    = rho0*(1.0_sp+M*th0)
                 omt0    = om0*t0
                 gamt0   = sin(Omega0*t0)
                 sin0    = sin(omt0+Beta*gamt0)
                 rhot(i) = sin0
               end do
               if(n<4) return
            end if
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,4
                 t0        = real(i,kind=dp)
                 th0       = -abs(cos(Omega0*t0))
                 rh00      = rho0*(1.0_dp+M*th0)
                 omt0      = om0*t0
                 gamt0     = sin(Omega0*t0)
                 sin0      = sin(omt0+Beta*gamt0)
                 rhot(i)   = sin0
                 t1        = real(i+1,kind=dp)
                 th1       = -abs(cos(Omega0*t1))
                 rh01      = rho0*(1.0_dp+M*th1)
                 omt1      = om0*t1
                 gamt1     = sin(Omega0*t1)
                 sin1      = sin(omt1+Beta*gamt1)
                 rhot(i+1) = sin1
                 t2        = real(i+2,kind=dp)
                 th2       = -abs(cos(Omega0*t2))
                 rh02      = rho0*(1.0_dp+M*th2)
                 omt2      = om0*t2
                 gamt2     = sin(Omega0*t2)
                 sin2      = sin(omt2+Beta*gamt2)
                 rhot(i+2) = sin2
                 t3        = real(i+3,kind=dp)
                 th3       = -abs(cos(Omega0*t3))
                 rh03      = rho0*(1.0_dp+M*th0)
                 omt3      = om0*t3
                 gamt3     = sin(Omega0*t3)
                 sin3      = sin(omt3+Beta*gamt3)
                 rhot(i+3) = sin3
                
               
            end do
       end subroutine raster_transmittance_unroll_4x_r8


       subroutine raster_transmittance_unroll_2x_r4(a,r,rho0,Omega0,Beta,R0,om0,rhot,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_transmittance_unroll_2x_r4
           !dir$ attributes forceinline ::   raster_transmittance_unroll_2x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_transmittance_unroll_2x_r4
           real(kind=sp),                 intent(in)  :: a
           real(kind=sp),                 intent(in)  :: r
           real(kind=sp),                 intent(in)  :: rho0
           real(kind=sp),                 intent(in)  :: Omega0
           real(kind=sp),                 intent(in)  :: Beta
           real(kind=sp),                 intent(in)  :: R0
           real(kind=sp),                 intent(in)  :: om0
           real(kind=sp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp), parameter :: c0 = 0.63661977236758134307553505349_sp
           real(kind=sp), automatic :: rho00,rho01
           real(kind=sp), automatic :: th0,th1
           real(kind=sp), automatic :: cos0,cos1
           real(kind=sp), automatic :: sin0,sin1
           real(kind=sp), automatic :: omt0,omt1
           real(kind=sp), automatic :: t0,t1
           real(kind=sp), automatic :: gamt0,gamt1
           real(kind=sp) :: M
           integer(kind=i4) :: i,m,m1
           M = c0*(a/(r+r))
           m = mod(n,2)
           if(m /= 0) then
              do i = 1,m
                 t0      = real(i,kind=sp)
                 th0     = -abs(cos(Omega0*t0))
                 rh00    = rho0*(1.0_sp+M*th0)
                 omt0    = om0*t0
                 gamt0   = sin(Omega0*t0)
                 sin0    = sin(omt0+Beta*gamt0)
                 rhot(i) = sin0
               end do
               if(n<2) return
            end if
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,2
                 t0        = real(i,kind=sp)
                 th0       = -abs(cos(Omega0*t0))
                 rh00      = rho0*(1.0_sp+M*th0)
                 omt0      = om0*t0
                 gamt0     = sin(Omega0*t0)
                 sin0      = sin(omt0+Beta*gamt0)
                 rhot(i)   = sin0
                 t1        = real(i+1,kind=sp)
                 th1       = -abs(cos(Omega0*t1))
                 rh01      = rho0*(1.0_sp+M*th1)
                 omt1      = om0*t1
                 gamt1     = sin(Omega0*t1)
                 sin1      = sin(omt1+Beta*gamt1)
                 rhot(i+1) = sin1
                             
            end do
       end subroutine raster_transmittance_unroll_2x_r4

       
       subroutine raster_transmittance_unroll_2x_r8(a,r,rho0,Omega0,Beta,R0,om0,rhot,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_transmittance_unroll_2x_r8
           !dir$ attributes forceinline ::   raster_transmittance_unroll_2x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_transmittance_unroll_2x_r8
           real(kind=dp),                 intent(in)  :: a
           real(kind=dp),                 intent(in)  :: r
           real(kind=dp),                 intent(in)  :: rho0
           real(kind=dp),                 intent(in)  :: Omega0
           real(kind=dp),                 intent(in)  :: Beta
           real(kind=dp),                 intent(in)  :: R0
           real(kind=dp),                 intent(in)  :: om0
           real(kind=dp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp), parameter :: c0 = 0.63661977236758134307553505349_dp
           real(kind=dp), automatic :: rho00,rho01
           real(kind=dp), automatic :: th0,th1
           real(kind=dp), automatic :: cos0,cos1
           real(kind=dp), automatic :: sin0,sin1
           real(kind=dp), automatic :: omt0,omt1
           real(kind=dp), automatic :: t0,t1
           real(kind=dp), automatic :: gamt0,gamt1
           real(kind=dp) :: M
           integer(kind=i4) :: i,m,m1
           M = c0*(a/(r+r))
           m = mod(n,2)
           if(m /= 0) then
              do i = 1,m
                 t0      = real(i,kind=dp)
                 th0     = -abs(cos(Omega0*t0))
                 rh00    = rho0*(1.0_sp+M*th0)
                 omt0    = om0*t0
                 gamt0   = sin(Omega0*t0)
                 sin0    = sin(omt0+Beta*gamt0)
                 rhot(i) = sin0
               end do
               if(n<2) return
            end if
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,2
                 t0        = real(i,kind=dp)
                 th0       = -abs(cos(Omega0*t0))
                 rh00      = rho0*(1.0_dp+M*th0)
                 omt0      = om0*t0
                 gamt0     = sin(Omega0*t0)
                 sin0      = sin(omt0+Beta*gamt0)
                 rhot(i)   = sin0
                 t1        = real(i+1,kind=dp)
                 th1       = -abs(cos(Omega0*t1))
                 rh01      = rho0*(1.0_dp+M*th1)
                 omt1      = om0*t1
                 gamt1     = sin(Omega0*t1)
                 sin1      = sin(omt1+Beta*gamt1)
                 rhot(i+1) = sin1
                
               
            end do
       end subroutine raster_transmittance_unroll_2x_r8


       subroutine raster_transmittance_r4(a,r,rho0,Omega0,Beta,R0,om0,rhot,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_transmittance_r4
           !dir$ attributes forceinline ::   raster_transmittance_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_transmittance_r4
           real(kind=sp),                 intent(in)  :: a
           real(kind=sp),                 intent(in)  :: r
           real(kind=sp),                 intent(in)  :: rho0
           real(kind=sp),                 intent(in)  :: Omega0
           real(kind=sp),                 intent(in)  :: Beta
           real(kind=sp),                 intent(in)  :: R0
           real(kind=sp),                 intent(in)  :: om0
           real(kind=sp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp), parameter :: c0 = 0.63661977236758134307553505349_sp
           real(kind=sp), automatic :: rho00
           real(kind=sp), automatic :: th0
           real(kind=sp), automatic :: cos0
           real(kind=sp), automatic :: sin0
           real(kind=sp), automatic :: omt0
           real(kind=sp), automatic :: t0
           real(kind=sp), automatic :: gamt0
           real(kind=sp) :: M
           integer(kind=i4) :: i
           M = c0*(a/(r+r))
         
            !dir$ assume_aligned rhot:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=1,n
                 t0        = real(i,kind=sp)
                 th0       = -abs(cos(Omega0*t0))
                 rh00      = rho0*(1.0_sp+M*th0)
                 omt0      = om0*t0
                 gamt0     = sin(Omega0*t0)
                 sin0      = sin(omt0+Beta*gamt0)
                 rhot(i)   = sin0
                            
            end do
       end subroutine raster_transmittance_r4


       subroutine raster_transmittance_r8(a,r,rho0,Omega0,Beta,R0,om0,rhot,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_transmittance_r8
           !dir$ attributes forceinline ::   raster_transmittance_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_transmittance_r8
           real(kind=dp),                 intent(in)  :: a
           real(kind=dp),                 intent(in)  :: r
           real(kind=dp),                 intent(in)  :: rho0
           real(kind=dp),                 intent(in)  :: Omega0
           real(kind=dp),                 intent(in)  :: Beta
           real(kind=dp),                 intent(in)  :: R0
           real(kind=dp),                 intent(in)  :: om0
           real(kind=dp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp), parameter :: c0 = 0.63661977236758134307553505349_dp
           real(kind=dp), automatic :: rho00
           real(kind=dp), automatic :: th0
           real(kind=dp), automatic :: cos0
           real(kind=dp), automatic :: sin0
           real(kind=dp), automatic :: omt0
           real(kind=dp), automatic :: t0
           real(kind=dp), automatic :: gamt0
           real(kind=dp) :: M
           integer(kind=i4) :: i
           M = c0*(a/(r+r))
         
            !dir$ assume_aligned rhot:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=1,n
                 t0        = real(i,kind=dp)
                 th0       = -abs(cos(Omega0*t0))
                 rh00      = rho0*(1.0_dp+M*th0)
                 omt0      = om0*t0
                 gamt0     = sin(Omega0*t0)
                 sin0      = sin(omt0+Beta*gamt0)
                 rhot(i)   = sin0
                            
            end do
       end subroutine raster_transmittance_r8


       subroutine raster_transmittance_exec_r4(a,r,rho0,Omega0,Beta,R0,om0,rhot,n,unroll_cnt)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: raster_transmittance_exec_r4
           real(kind=sp),                 intent(in)  :: a
           real(kind=sp),                 intent(in)  :: r
           real(kind=sp),                 intent(in)  :: rho0
           real(kind=sp),                 intent(in)  :: Omega0
           real(kind=sp),                 intent(in)  :: Beta
           real(kind=sp),                 intent(in)  :: R0
           real(kind=sp),                 intent(in)  :: om0
           real(kind=sp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           integer(kind=i4),              intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                call raster_transmittance_unroll_16x_r4(a,r,rho0,Omega0,Beta,R0,om0,rhot,n)
              case (8)
                call raster_transmittance_unroll_8x_r4(a,r,rho0,Omega0,Beta,R0,om0,rhot,n)
              case (4)
                call raster_transmittance_unroll_4x_r4(a,r,rho0,Omega0,Beta,R0,om0,rhot,n)
              case (2)
                call raster_transmittance_unroll_2x_r4(a,r,rho0,Omega0,Beta,R0,om0,rhot,n)
              case (0)
                call raster_transmittance_r4(a,r,rho0,Omega0,Beta,R0,om0,rhot,n)
              case default
                return
              end select
       end subroutine raster_transmittance_exec_r4


       subroutine raster_transmittance_exec_r8(a,r,rho0,Omega0,Beta,R0,om0,rhot,n,unroll_cnt)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: raster_transmittance_exec_r8
           real(kind=dp),                 intent(in)  :: a
           real(kind=dp),                 intent(in)  :: r
           real(kind=dp),                 intent(in)  :: rho0
           real(kind=dp),                 intent(in)  :: Omega0
           real(kind=dp),                 intent(in)  :: Beta
           real(kind=dp),                 intent(in)  :: R0
           real(kind=dp),                 intent(in)  :: om0
           real(kind=dp), dimension(1:n), intent(out) :: rhot
           integer(kind=i4),              intent(in)  :: n
           integer(kind=i4),              intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                call raster_transmittance_unroll_16x_r8(a,r,rho0,Omega0,Beta,R0,om0,rhot,n)
              case (8)
                call raster_transmittance_unroll_8x_r8(a,r,rho0,Omega0,Beta,R0,om0,rhot,n)
              case (4)
                call raster_transmittance_unroll_4x_r8(a,r,rho0,Omega0,Beta,R0,om0,rhot,n)
              case (2)
                call raster_transmittance_unroll_2x_r8(a,r,rho0,Omega0,Beta,R0,om0,rhot,n)
              case (0)
                call raster_transmittance_r8(a,r,rho0,Omega0,Beta,R0,om0,rhot,n)
              case default
                return
              end select
       end subroutine raster_transmittance_exec_r8



       !Гармоническая частотная модуляция коэффициента 
       !пропускания.
       !Formula 1, p. 196
       subroutine transmitt_hfreq_mod_unroll_16x_r4(Phi0,rho0,om0,Beta,Omega0,Phiom,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmitt_hfreq_mod_unroll_16x_r4
           !dir$ attributes forceinline ::   transmitt_hfreq_mod_unroll_16x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmitt_hfreq_mod_unroll_16x_r4
           real(kind=sp),                     intent(in) :: Phi0
           real(kind=sp),                     intent(in) :: rho0
           real(kind=sp),                     intent(in) :: om0
           real(kind=sp),                     intent(in) :: Beta
           real(kind=sp),                     intent(in) :: Omega0
           real(kind=sp), dimension(1:n),     intent(out):: Phiom
           integer(kind=i4),                  intent(in) :: n
           real(kind=sp), automatic :: omt0,omt1,omt2,omt3,omt4,omt5,omt6,omt7
           real(kind=sp), automatic :: omt8,omt9,omt10,omt11,omt12,omt13,omt14,omt15
           real(kind=sp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15
           real(kind=sp), automatic :: somt0,somt1,somt2,somt3,somt4,somt5,somt6,somt7
           real(kind=sp), automatic :: somt7,somt8,somt9,somt10,somt11,somt12,somt13,somt14,somt15
           real(kind=sp), automatic :: tB0,tB1,tB2,tB3,tB4,tB5,tB6,tB7
           real(kind=sp), automatic :: tB8,tB9,tB10,tB11,tB12,tB13,tB14,tB15
           real(kind=sp), automatic :: term0,term1,tetm2,term3,term4,term5,term6,term7
           real(kind=sp), automatic :: term8,term9,term10,term11,term12,term13,term14,term15
           real(kind=sp), automatic :: c0,c1,c2,c3,c4,c5,c6,c7
           real(kind=sp), automatic :: c8,c9,c10,c11,c12,c13,c14,c15
           real(kind=sp) :: B2,oma,oms,Phiro,soma,soms
           integer(kind=sp) :: i,m,m1
           B2    = Beta*0.5_sp
           oma   = om0+Omega0
           soma  = sin(oma)
           oms   = om0-Omega0
           soms  = sin(oms)
           Phiro = Phi0*rho0
           m  = mod(n,16)
           if(m /= 0) then
              do i=1,m
                 t0     = real(i,kind=sp)
                 omt0   = om0*t0
                 somt0  = sin(omt0)
                 tB0    = t0-B2
                 term0  = B2*soma*tB0
                 c0     = soms-t0
                 Phiom(i) = Phir0*(1.0_sp+somt0+term0*c0)
              end do
              if(n<16) return
            end if
            m1 = m+1
            !dir$ assume_aligned Phiom:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,16
                 t0         = real(i,kind=sp)
                 omt0       = om0*t0
                 somt0      = sin(omt0)
                 tB0        = t0-B2
                 term0      = B2*soma*tB0
                 c0         = soms-t0
                 Phiom(i)   = Phir0*(1.0_sp+somt0+term0*c0)
                 t1         = real(i+1,kind=sp)
                 omt1       = om0*t1
                 somt1      = sin(omt1)
                 tB1        = t1-B2
                 term1      = B2*soma*tB1
                 c1         = soms-t1
                 Phiom(i+1) = Phir0*(1.0_sp+somt1+term1*c1)
                 t2         = real(i+2,kind=sp)
                 omt2       = om0*t2
                 somt2      = sin(omt2)
                 tB2        = t2-B2
                 term2      = B2*soma*tB2
                 c2         = soms-t2
                 Phiom(i+2) = Phir0*(1.0_sp+somt2+term2*c2)
                 t3         = real(i+3,kind=sp)
                 omt3       = om0*t3
                 somt3      = sin(omt3)
                 tB3        = t3-B2
                 term3      = B2*soma*tB3
                 c3         = soms-t3
                 Phiom(i+3) = Phir0*(1.0_sp+somt3+term3*c3)
                 t4         = real(i+4,kind=sp)
                 omt4       = om0*t4
                 somt4      = sin(omt4)
                 tB4        = t4-B2
                 term4      = B2*soma*tB4
                 c4         = soms-t4
                 Phiom(i+4) = Phir0*(1.0_sp+somt4+term4*c4)
                 t5         = real(i+5,kind=sp)
                 omt5       = om0*t5
                 somt5      = sin(omt5)
                 tB5        = t5-B2
                 term5      = B2*soma*tB5
                 c5         = soms-t5
                 Phiom(i+5) = Phir0*(1.0_sp+somt5+term5*c5)
                 t6         = real(i+6,kind=sp)
                 omt6       = om0*t6
                 somt6      = sin(omt6)
                 tB6        = t6-B2
                 term6      = B2*soma*tB6
                 c6         = soms-t6
                 Phiom(i+6) = Phir0*(1.0_sp+somt6+term6*c6)
                 t7         = real(i+7,kind=sp)
                 omt7       = om0*t7
                 somt7      = sin(omt7)
                 tB7        = t7-B2
                 term7      = B2*soma*tB7
                 c7         = soms-t7
                 Phiom(i+7) = Phir0*(1.0_sp+somt7+term7*c7)
                 t8         = real(i+8,kind=sp)
                 omt8       = om0*t8
                 somt8      = sin(omt8)
                 tB8        = t8-B2
                 term8      = B2*soma*tB8
                 c8         = soms-t8
                 Phiom(i+8) = Phir0*(1.0_sp+somt8+term8*c8) 
                 t9         = real(i+9,kind=sp)
                 omt9       = om0*t9
                 somt9      = sin(omt9)
                 tB9        = t9-B2
                 term9      = B2*soma*tB9
                 c9         = soms-t9
                 Phiom(i+9) = Phir0*(1.0_sp+somt9+term9*c9)
                 t10        = real(i+10,kind=sp)
                 omt10      = om0*t10
                 somt10     = sin(omt10)
                 tB10       = t10-B2
                 term10     = B2*soma*tB10
                 c10        = soms-t10
                 Phiom(i+10)= Phir0*(1.0_sp+somt10+term10*c10)
                 t11        = real(i+11,kind=sp)
                 omt11      = om0*t11
                 somt11     = sin(omt11)
                 tB11       = t11-B2
                 term11     = B2*soma*tB11
                 c11        = soms-t11
                 Phiom(i+11)= Phir0*(1.0_sp+somt11+term11*c11)
                 t12        = real(i+12,kind=sp)
                 omt12      = om0*t12
                 somt12     = sin(omt12)
                 tB12       = t12-B2
                 term12     = B2*soma*tB12
                 c12        = soms-t12
                 Phiom(i+12)= Phir0*(1.0_sp+somt12+term12*c12)
                 t13        = real(i+13,kind=sp)
                 omt13      = om0*t13
                 somt13     = sin(omt13)
                 tB13       = t13-B2
                 term13     = B2*soma*tB13
                 c13        = soms-t13
                 Phiom(i+13)= Phir0*(1.0_sp+somt13+term13*c13) 
                 t14        = real(i+14,kind=sp)
                 omt14      = om0*t14
                 somt14     = sin(omt14)
                 tB14       = t14-B2
                 term14     = B2*soma*tB14
                 c14        = soms-t14
                 Phiom(i+14)= Phir0*(1.0_sp+somt14+term14*c14)
                 t15        = real(i+15,kind=sp)
                 omt15      = om0*t15
                 somt15     = sin(omt15)
                 tB15       = t15-B2
                 term15     = B2*soma*tB15
                 c15        = soms-t15
                 Phiom(i+15)= Phir0*(1.0_sp+somt15+term15*c15)
            end do
       end subroutine transmitt_hfreq_mod_unroll_16x_r4
 

       subroutine transmitt_hfreq_mod_unroll_16x_r8(Phi0,rho0,om0,Beta,Omega0,Phiom,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmitt_hfreq_mod_unroll_16x_r8
           !dir$ attributes forceinline ::   transmitt_hfreq_mod_unroll_16x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmitt_hfreq_mod_unroll_16x_r8
           real(kind=dp),                     intent(in) :: Phi0
           real(kind=dp),                     intent(in) :: rho0
           real(kind=dp),                     intent(in) :: om0
           real(kind=dp),                     intent(in) :: Beta
           real(kind=dp),                     intent(in) :: Omega0
           real(kind=dp), dimension(1:n),     intent(out):: Phiom
           integer(kind=i4),                  intent(in) :: n
           real(kind=dp), automatic :: omt0,omt1,omt2,omt3,omt4,omt5,omt6,omt7
           real(kind=dp), automatic :: omt8,omt9,omt10,omt11,omt12,omt13,omt14,omt15
           real(kind=dp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15
           real(kind=dp), automatic :: somt0,somt1,somt2,somt3,somt4,somt5,somt6,somt7
           real(kind=dp), automatic :: somt7,somt8,somt9,somt10,somt11,somt12,somt13,somt14,somt15
           real(kind=dp), automatic :: tB0,tB1,tB2,tB3,tB4,tB5,tB6,tB7
           real(kind=dp), automatic :: tB8,tB9,tB10,tB11,tB12,tB13,tB14,tB15
           real(kind=dp), automatic :: term0,term1,tetm2,term3,term4,term5,term6,term7
           real(kind=dp), automatic :: term8,term9,term10,term11,term12,term13,term14,term15
           real(kind=dp), automatic :: c0,c1,c2,c3,c4,c5,c6,c7
           real(kind=dp), automatic :: c8,c9,c10,c11,c12,c13,c14,c15
           real(kind=dp) :: B2,oma,oms,Phiro,soma,soms
           integer(kind=sp) :: i,m,m1
           B2    = Beta*0.5_dp
           oma   = om0+Omega0
           soma  = sin(oma)
           oms   = om0-Omega0
           soms  = sin(oms)
           Phiro = Phi0*rho0
           m  = mod(n,16)
           if(m /= 0) then
              do i=1,m
                 t0     = real(i,kind=dp)
                 omt0   = om0*t0
                 somt0  = sin(omt0)
                 tB0    = t0-B2
                 term0  = B2*soma*tB0
                 c0     = soms-t0
                 Phiom(i) = Phir0*(1.0_dp+somt0+term0*c0)
              end do
              if(n<16) return
            end if
            m1 = m+1
            !dir$ assume_aligned Phiom:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,16
                 t0         = real(i,kind=dp)
                 omt0       = om0*t0
                 somt0      = sin(omt0)
                 tB0        = t0-B2
                 term0      = B2*soma*tB0
                 c0         = soms-t0
                 Phiom(i)   = Phir0*(1.0_dp+somt0+term0*c0)
                 t1         = real(i+1,kind=dp)
                 omt1       = om0*t1
                 somt1      = sin(omt1)
                 tB1        = t1-B2
                 term1      = B2*soma*tB1
                 c1         = soms-t1
                 Phiom(i+1) = Phir0*(1.0_dp+somt1+term1*c1)
                 t2         = real(i+2,kind=dp)
                 omt2       = om0*t2
                 somt2      = sin(omt2)
                 tB2        = t2-B2
                 term2      = B2*soma*tB2
                 c2         = soms-t2
                 Phiom(i+2) = Phir0*(1.0_dp+somt2+term2*c2)
                 t3         = real(i+3,kind=dp)
                 omt3       = om0*t3
                 somt3      = sin(omt3)
                 tB3        = t3-B2
                 term3      = B2*soma*tB3
                 c3         = soms-t3
                 Phiom(i+3) = Phir0*(1.0_dp+somt3+term3*c3)
                 t4         = real(i+4,kind=dp)
                 omt4       = om0*t4
                 somt4      = sin(omt4)
                 tB4        = t4-B2
                 term4      = B2*soma*tB4
                 c4         = soms-t4
                 Phiom(i+4) = Phir0*(1.0_dp+somt4+term4*c4)
                 t5         = real(i+5,kind=dp)
                 omt5       = om0*t5
                 somt5      = sin(omt5)
                 tB5        = t5-B2
                 term5      = B2*soma*tB5
                 c5         = soms-t5
                 Phiom(i+5) = Phir0*(1.0_dp+somt5+term5*c5)
                 t6         = real(i+6,kind=dp)
                 omt6       = om0*t6
                 somt6      = sin(omt6)
                 tB6        = t6-B2
                 term6      = B2*soma*tB6
                 c6         = soms-t6
                 Phiom(i+6) = Phir0*(1.0_dp+somt6+term6*c6)
                 t7         = real(i+7,kind=dp)
                 omt7       = om0*t7
                 somt7      = sin(omt7)
                 tB7        = t7-B2
                 term7      = B2*soma*tB7
                 c7         = soms-t7
                 Phiom(i+7) = Phir0*(1.0_dp+somt7+term7*c7)
                 t8         = real(i+8,kind=dp)
                 omt8       = om0*t8
                 somt8      = sin(omt8)
                 tB8        = t8-B2
                 term8      = B2*soma*tB8
                 c8         = soms-t8
                 Phiom(i+8) = Phir0*(1.0_dp+somt8+term8*c8) 
                 t9         = real(i+9,kind=dp)
                 omt9       = om0*t9
                 somt9      = sin(omt9)
                 tB9        = t9-B2
                 term9      = B2*soma*tB9
                 c9         = soms-t9
                 Phiom(i+9) = Phir0*(1.0_dp+somt9+term9*c9)
                 t10        = real(i+10,kind=dp)
                 omt10      = om0*t10
                 somt10     = sin(omt10)
                 tB10       = t10-B2
                 term10     = B2*soma*tB10
                 c10        = soms-t10
                 Phiom(i+10)= Phir0*(1.0_dp+somt10+term10*c10)
                 t11        = real(i+11,kind=dp)
                 omt11      = om0*t11
                 somt11     = sin(omt11)
                 tB11       = t11-B2
                 term11     = B2*soma*tB11
                 c11        = soms-t11
                 Phiom(i+11)= Phir0*(1.0_dp+somt11+term11*c11)
                 t12        = real(i+12,kind=dp)
                 omt12      = om0*t12
                 somt12     = sin(omt12)
                 tB12       = t12-B2
                 term12     = B2*soma*tB12
                 c12        = soms-t12
                 Phiom(i+12)= Phir0*(1.0_dp+somt12+term12*c12)
                 t13        = real(i+13,kind=dp)
                 omt13      = om0*t13
                 somt13     = sin(omt13)
                 tB13       = t13-B2
                 term13     = B2*soma*tB13
                 c13        = soms-t13
                 Phiom(i+13)= Phir0*(1.0_dp+somt13+term13*c13) 
                 t14        = real(i+14,kind=dp)
                 omt14      = om0*t14
                 somt14     = sin(omt14)
                 tB14       = t14-B2
                 term14     = B2*soma*tB14
                 c14        = soms-t14
                 Phiom(i+14)= Phir0*(1.0_dp+somt14+term14*c14)
                 t15        = real(i+15,kind=dp)
                 omt15      = om0*t15
                 somt15     = sin(omt15)
                 tB15       = t15-B2
                 term15     = B2*soma*tB15
                 c15        = soms-t15
                 Phiom(i+15)= Phir0*(1.0_dp+somt15+term15*c15)
            end do
       end subroutine transmitt_hfreq_mod_unroll_16x_r8


       subroutine transmitt_hfreq_mod_unroll_8x_r4(Phi0,rho0,om0,Beta,Omega0,Phiom,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmitt_hfreq_mod_unroll_8x_r4
           !dir$ attributes forceinline ::   transmitt_hfreq_mod_unroll_8x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmitt_hfreq_mod_unroll_8x_r4
           real(kind=sp),                     intent(in) :: Phi0
           real(kind=sp),                     intent(in) :: rho0
           real(kind=sp),                     intent(in) :: om0
           real(kind=sp),                     intent(in) :: Beta
           real(kind=sp),                     intent(in) :: Omega0
           real(kind=sp), dimension(1:n),     intent(out):: Phiom
           integer(kind=i4),                  intent(in) :: n
           real(kind=sp), automatic :: omt0,omt1,omt2,omt3,omt4,omt5,omt6,omt7
           real(kind=sp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=sp), automatic :: somt0,somt1,somt2,somt3,somt4,somt5,somt6,somt7
           real(kind=sp), automatic :: tB0,tB1,tB2,tB3,tB4,tB5,tB6,tB7
           real(kind=sp), automatic :: term0,term1,tetm2,term3,term4,term5,term6,term7
           real(kind=sp), automatic :: c0,c1,c2,c3,c4,c5,c6,c7
           real(kind=sp) :: B2,oma,oms,Phiro,soma,soms
           integer(kind=sp) :: i,m,m1
           B2    = Beta*0.5_sp
           oma   = om0+Omega0
           soma  = sin(oma)
           oms   = om0-Omega0
           soms  = sin(oms)
           Phiro = Phi0*rho0
           m  = mod(n,8)
           if(m /= 0) then
              do i=1,m
                 t0     = real(i,kind=sp)
                 omt0   = om0*t0
                 somt0  = sin(omt0)
                 tB0    = t0-B2
                 term0  = B2*soma*tB0
                 c0     = soms-t0
                 Phiom(i) = Phir0*(1.0_sp+somt0+term0*c0)
              end do
              if(n<8) return
            end if
            m1 = m+1
            !dir$ assume_aligned Phiom:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,8
                 t0         = real(i,kind=sp)
                 omt0       = om0*t0
                 somt0      = sin(omt0)
                 tB0        = t0-B2
                 term0      = B2*soma*tB0
                 c0         = soms-t0
                 Phiom(i)   = Phir0*(1.0_sp+somt0+term0*c0)
                 t1         = real(i+1,kind=sp)
                 omt1       = om0*t1
                 somt1      = sin(omt1)
                 tB1        = t1-B2
                 term1      = B2*soma*tB1
                 c1         = soms-t1
                 Phiom(i+1) = Phir0*(1.0_sp+somt1+term1*c1)
                 t2         = real(i+2,kind=sp)
                 omt2       = om0*t2
                 somt2      = sin(omt2)
                 tB2        = t2-B2
                 term2      = B2*soma*tB2
                 c2         = soms-t2
                 Phiom(i+2) = Phir0*(1.0_sp+somt2+term2*c2)
                 t3         = real(i+3,kind=sp)
                 omt3       = om0*t3
                 somt3      = sin(omt3)
                 tB3        = t3-B2
                 term3      = B2*soma*tB3
                 c3         = soms-t3
                 Phiom(i+3) = Phir0*(1.0_sp+somt3+term3*c3)
                 t4         = real(i+4,kind=sp)
                 omt4       = om0*t4
                 somt4      = sin(omt4)
                 tB4        = t4-B2
                 term4      = B2*soma*tB4
                 c4         = soms-t4
                 Phiom(i+4) = Phir0*(1.0_sp+somt4+term4*c4)
                 t5         = real(i+5,kind=sp)
                 omt5       = om0*t5
                 somt5      = sin(omt5)
                 tB5        = t5-B2
                 term5      = B2*soma*tB5
                 c5         = soms-t5
                 Phiom(i+5) = Phir0*(1.0_sp+somt5+term5*c5)
                 t6         = real(i+6,kind=sp)
                 omt6       = om0*t6
                 somt6      = sin(omt6)
                 tB6        = t6-B2
                 term6      = B2*soma*tB6
                 c6         = soms-t6
                 Phiom(i+6) = Phir0*(1.0_sp+somt6+term6*c6)
                 t7         = real(i+7,kind=sp)
                 omt7       = om0*t7
                 somt7      = sin(omt7)
                 tB7        = t7-B2
                 term7      = B2*soma*tB7
                 c7         = soms-t7
                 Phiom(i+7) = Phir0*(1.0_sp+somt7+term7*c7)
             end do
       end subroutine transmitt_hfreq_mod_unroll_8x_r4


       subroutine transmitt_hfreq_mod_unroll_8x_r8(Phi0,rho0,om0,Beta,Omega0,Phiom,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmitt_hfreq_mod_unroll_8x_r8
           !dir$ attributes forceinline ::   transmitt_hfreq_mod_unroll_8x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmitt_hfreq_mod_unroll_8x_r8
           real(kind=dp),                     intent(in) :: Phi0
           real(kind=dp),                     intent(in) :: rho0
           real(kind=dp),                     intent(in) :: om0
           real(kind=dp),                     intent(in) :: Beta
           real(kind=dp),                     intent(in) :: Omega0
           real(kind=dp), dimension(1:n),     intent(out):: Phiom
           integer(kind=i4),                  intent(in) :: n
           real(kind=dp), automatic :: omt0,omt1,omt2,omt3,omt4,omt5,omt6,omt7
           real(kind=dp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=dp), automatic :: somt0,somt1,somt2,somt3,somt4,somt5,somt6,somt7
           real(kind=dp), automatic :: tB0,tB1,tB2,tB3,tB4,tB5,tB6,tB7
           real(kind=dp), automatic :: term0,term1,tetm2,term3,term4,term5,term6,term7
           real(kind=dp), automatic :: c0,c1,c2,c3,c4,c5,c6,c7
           real(kind=dp) :: B2,oma,oms,Phiro,soma,soms
           integer(kind=sp) :: i,m,m1
           B2    = Beta*0.5_dp
           oma   = om0+Omega0
           soma  = sin(oma)
           oms   = om0-Omega0
           soms  = sin(oms)
           Phiro = Phi0*rho0
           m  = mod(n,8)
           if(m /= 0) then
              do i=1,m
                 t0     = real(i,kind=dp)
                 omt0   = om0*t0
                 somt0  = sin(omt0)
                 tB0    = t0-B2
                 term0  = B2*soma*tB0
                 c0     = soms-t0
                 Phiom(i) = Phir0*(1.0_dp+somt0+term0*c0)
              end do
              if(n<8) return
            end if
            m1 = m+1
            !dir$ assume_aligned Phiom:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,8
                 t0         = real(i,kind=dp)
                 omt0       = om0*t0
                 somt0      = sin(omt0)
                 tB0        = t0-B2
                 term0      = B2*soma*tB0
                 c0         = soms-t0
                 Phiom(i)   = Phir0*(1.0_dp+somt0+term0*c0)
                 t1         = real(i+1,kind=dp)
                 omt1       = om0*t1
                 somt1      = sin(omt1)
                 tB1        = t1-B2
                 term1      = B2*soma*tB1
                 c1         = soms-t1
                 Phiom(i+1) = Phir0*(1.0_dp+somt1+term1*c1)
                 t2         = real(i+2,kind=dp)
                 omt2       = om0*t2
                 somt2      = sin(omt2)
                 tB2        = t2-B2
                 term2      = B2*soma*tB2
                 c2         = soms-t2
                 Phiom(i+2) = Phir0*(1.0_dp+somt2+term2*c2)
                 t3         = real(i+3,kind=dp)
                 omt3       = om0*t3
                 somt3      = sin(omt3)
                 tB3        = t3-B2
                 term3      = B2*soma*tB3
                 c3         = soms-t3
                 Phiom(i+3) = Phir0*(1.0_dp+somt3+term3*c3)
                 t4         = real(i+4,kind=dp)
                 omt4       = om0*t4
                 somt4      = sin(omt4)
                 tB4        = t4-B2
                 term4      = B2*soma*tB4
                 c4         = soms-t4
                 Phiom(i+4) = Phir0*(1.0_dp+somt4+term4*c4)
                 t5         = real(i+5,kind=dp)
                 omt5       = om0*t5
                 somt5      = sin(omt5)
                 tB5        = t5-B2
                 term5      = B2*soma*tB5
                 c5         = soms-t5
                 Phiom(i+5) = Phir0*(1.0_dp+somt5+term5*c5)
                 t6         = real(i+6,kind=dp)
                 omt6       = om0*t6
                 somt6      = sin(omt6)
                 tB6        = t6-B2
                 term6      = B2*soma*tB6
                 c6         = soms-t6
                 Phiom(i+6) = Phir0*(1.0_dp+somt6+term6*c6)
                 t7         = real(i+7,kind=dp)
                 omt7       = om0*t7
                 somt7      = sin(omt7)
                 tB7        = t7-B2
                 term7      = B2*soma*tB7
                 c7         = soms-t7
                 Phiom(i+7) = Phir0*(1.0_dp+somt7+term7*c7)
                   
            end do
       end subroutine transmitt_hfreq_mod_unroll_8x_r8


       
       subroutine transmitt_hfreq_mod_unroll_4x_r4(Phi0,rho0,om0,Beta,Omega0,Phiom,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmitt_hfreq_mod_unroll_4x_r4
           !dir$ attributes forceinline ::   transmitt_hfreq_mod_unroll_4x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmitt_hfreq_mod_unroll_4x_r4
           real(kind=sp),                     intent(in) :: Phi0
           real(kind=sp),                     intent(in) :: rho0
           real(kind=sp),                     intent(in) :: om0
           real(kind=sp),                     intent(in) :: Beta
           real(kind=sp),                     intent(in) :: Omega0
           real(kind=sp), dimension(1:n),     intent(out):: Phiom
           integer(kind=i4),                  intent(in) :: n
           real(kind=sp), automatic :: omt0,omt1,omt2,omt3
           real(kind=sp), automatic :: t0,t1,t2,t3
           real(kind=sp), automatic :: somt0,somt1,somt2,somt3
           real(kind=sp), automatic :: tB0,tB1,tB2,tB3
           real(kind=sp), automatic :: term0,term1,tetm2,term3
           real(kind=sp), automatic :: c0,c1,c2,c3
           real(kind=sp) :: B2,oma,oms,Phiro,soma,soms
           integer(kind=sp) :: i,m,m1
           B2    = Beta*0.5_sp
           oma   = om0+Omega0
           soma  = sin(oma)
           oms   = om0-Omega0
           soms  = sin(oms)
           Phiro = Phi0*rho0
           m  = mod(n,4)
           if(m /= 0) then
              do i=1,m
                 t0     = real(i,kind=sp)
                 omt0   = om0*t0
                 somt0  = sin(omt0)
                 tB0    = t0-B2
                 term0  = B2*soma*tB0
                 c0     = soms-t0
                 Phiom(i) = Phir0*(1.0_sp+somt0+term0*c0)
              end do
              if(n<4) return
            end if
            m1 = m+1
            !dir$ assume_aligned Phiom:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,4
                 t0         = real(i,kind=sp)
                 omt0       = om0*t0
                 somt0      = sin(omt0)
                 tB0        = t0-B2
                 term0      = B2*soma*tB0
                 c0         = soms-t0
                 Phiom(i)   = Phir0*(1.0_sp+somt0+term0*c0)
                 t1         = real(i+1,kind=sp)
                 omt1       = om0*t1
                 somt1      = sin(omt1)
                 tB1        = t1-B2
                 term1      = B2*soma*tB1
                 c1         = soms-t1
                 Phiom(i+1) = Phir0*(1.0_sp+somt1+term1*c1)
                 t2         = real(i+2,kind=sp)
                 omt2       = om0*t2
                 somt2      = sin(omt2)
                 tB2        = t2-B2
                 term2      = B2*soma*tB2
                 c2         = soms-t2
                 Phiom(i+2) = Phir0*(1.0_sp+somt2+term2*c2)
                 t3         = real(i+3,kind=sp)
                 omt3       = om0*t3
                 somt3      = sin(omt3)
                 tB3        = t3-B2
                 term3      = B2*soma*tB3
                 c3         = soms-t3
                 Phiom(i+3) = Phir0*(1.0_sp+somt3+term3*c3)
            
             end do
       end subroutine transmitt_hfreq_mod_unroll_4x_r4


       subroutine transmitt_hfreq_mod_unroll_4x_r8(Phi0,rho0,om0,Beta,Omega0,Phiom,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmitt_hfreq_mod_unroll_4x_r8
           !dir$ attributes forceinline ::   transmitt_hfreq_mod_unroll_4x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmitt_hfreq_mod_unroll_4x_r8
           real(kind=dp),                     intent(in) :: Phi0
           real(kind=dp),                     intent(in) :: rho0
           real(kind=dp),                     intent(in) :: om0
           real(kind=dp),                     intent(in) :: Beta
           real(kind=dp),                     intent(in) :: Omega0
           real(kind=dp), dimension(1:n),     intent(out):: Phiom
           integer(kind=i4),                  intent(in) :: n
           real(kind=dp), automatic :: omt0,omt1,omt2,omt3
           real(kind=dp), automatic :: t0,t1,t2,t3
           real(kind=dp), automatic :: somt0,somt1,somt2,somt3
           real(kind=dp), automatic :: tB0,tB1,tB2,tB3
           real(kind=dp), automatic :: term0,term1,tetm2,term3
           real(kind=dp), automatic :: c0,c1,c2,c3
           real(kind=dp) :: B2,oma,oms,Phiro,soma,soms
           integer(kind=sp) :: i,m,m1
           B2    = Beta*0.5_dp
           oma   = om0+Omega0
           soma  = sin(oma)
           oms   = om0-Omega0
           soms  = sin(oms)
           Phiro = Phi0*rho0
           m  = mod(n,4)
           if(m /= 0) then
              do i=1,m
                 t0     = real(i,kind=dp)
                 omt0   = om0*t0
                 somt0  = sin(omt0)
                 tB0    = t0-B2
                 term0  = B2*soma*tB0
                 c0     = soms-t0
                 Phiom(i) = Phir0*(1.0_dp+somt0+term0*c0)
              end do
              if(n<4) return
            end if
            m1 = m+1
            !dir$ assume_aligned Phiom:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,4
                 t0         = real(i,kind=dp)
                 omt0       = om0*t0
                 somt0      = sin(omt0)
                 tB0        = t0-B2
                 term0      = B2*soma*tB0
                 c0         = soms-t0
                 Phiom(i)   = Phir0*(1.0_dp+somt0+term0*c0)
                 t1         = real(i+1,kind=dp)
                 omt1       = om0*t1
                 somt1      = sin(omt1)
                 tB1        = t1-B2
                 term1      = B2*soma*tB1
                 c1         = soms-t1
                 Phiom(i+1) = Phir0*(1.0_dp+somt1+term1*c1)
                 t2         = real(i+2,kind=dp)
                 omt2       = om0*t2
                 somt2      = sin(omt2)
                 tB2        = t2-B2
                 term2      = B2*soma*tB2
                 c2         = soms-t2
                 Phiom(i+2) = Phir0*(1.0_dp+somt2+term2*c2)
                 t3         = real(i+3,kind=dp)
                 omt3       = om0*t3
                 somt3      = sin(omt3)
                 tB3        = t3-B2
                 term3      = B2*soma*tB3
                 c3         = soms-t3
                 Phiom(i+3) = Phir0*(1.0_dp+somt3+term3*c3)                
            end do
       end subroutine transmitt_hfreq_mod_unroll_4x_r8


       subroutine transmitt_hfreq_mod_unroll_2x_r4(Phi0,rho0,om0,Beta,Omega0,Phiom,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmitt_hfreq_mod_unroll_2x_r4
           !dir$ attributes forceinline ::   transmitt_hfreq_mod_unroll_2x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmitt_hfreq_mod_unroll_2x_r4
           real(kind=sp),                     intent(in) :: Phi0
           real(kind=sp),                     intent(in) :: rho0
           real(kind=sp),                     intent(in) :: om0
           real(kind=sp),                     intent(in) :: Beta
           real(kind=sp),                     intent(in) :: Omega0
           real(kind=sp), dimension(1:n),     intent(out):: Phiom
           integer(kind=i4),                  intent(in) :: n
           real(kind=sp), automatic :: omt0,omt1
           real(kind=sp), automatic :: t0,t1
           real(kind=sp), automatic :: somt0,somt1
           real(kind=sp), automatic :: tB0,tB1
           real(kind=sp), automatic :: term0,term1
           real(kind=sp), automatic :: c0,c1
           real(kind=sp) :: B2,oma,oms,Phiro,soma,soms
           integer(kind=sp) :: i,m,m1
           B2    = Beta*0.5_sp
           oma   = om0+Omega0
           soma  = sin(oma)
           oms   = om0-Omega0
           soms  = sin(oms)
           Phiro = Phi0*rho0
           m  = mod(n,2)
           if(m /= 0) then
              do i=1,m
                 t0     = real(i,kind=sp)
                 omt0   = om0*t0
                 somt0  = sin(omt0)
                 tB0    = t0-B2
                 term0  = B2*soma*tB0
                 c0     = soms-t0
                 Phiom(i) = Phir0*(1.0_sp+somt0+term0*c0)
              end do
              if(n<2) return
            end if
            m1 = m+1
            !dir$ assume_aligned Phiom:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,2
                 t0         = real(i,kind=sp)
                 omt0       = om0*t0
                 somt0      = sin(omt0)
                 tB0        = t0-B2
                 term0      = B2*soma*tB0
                 c0         = soms-t0
                 Phiom(i)   = Phir0*(1.0_sp+somt0+term0*c0)
                 t1         = real(i+1,kind=sp)
                 omt1       = om0*t1
                 somt1      = sin(omt1)
                 tB1        = t1-B2
                 term1      = B2*soma*tB1
                 c1         = soms-t1
                 Phiom(i+1) = Phir0*(1.0_sp+somt1+term1*c1)
                           
             end do
       end subroutine transmitt_hfreq_mod_unroll_2x_r4


       subroutine transmitt_hfreq_mod_unroll_2x_r8(Phi0,rho0,om0,Beta,Omega0,Phiom,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmitt_hfreq_mod_unroll_2x_r8
           !dir$ attributes forceinline ::   transmitt_hfreq_mod_unroll_2x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmitt_hfreq_mod_unroll_2x_r8
           real(kind=dp),                     intent(in) :: Phi0
           real(kind=dp),                     intent(in) :: rho0
           real(kind=dp),                     intent(in) :: om0
           real(kind=dp),                     intent(in) :: Beta
           real(kind=dp),                     intent(in) :: Omega0
           real(kind=dp), dimension(1:n),     intent(out):: Phiom
           integer(kind=i4),                  intent(in) :: n
           real(kind=dp), automatic :: omt0,omt1
           real(kind=dp), automatic :: t0,t1
           real(kind=dp), automatic :: somt0,somt1
           real(kind=dp), automatic :: tB0,tB1
           real(kind=dp), automatic :: term0,term1
           real(kind=dp), automatic :: c0,c1
           real(kind=dp) :: B2,oma,oms,Phiro,soma,soms
           integer(kind=sp) :: i,m,m1
           B2    = Beta*0.5_dp
           oma   = om0+Omega0
           soma  = sin(oma)
           oms   = om0-Omega0
           soms  = sin(oms)
           Phiro = Phi0*rho0
           m  = mod(n,2)
           if(m /= 0) then
              do i=1,m
                 t0     = real(i,kind=dp)
                 omt0   = om0*t0
                 somt0  = sin(omt0)
                 tB0    = t0-B2
                 term0  = B2*soma*tB0
                 c0     = soms-t0
                 Phiom(i) = Phir0*(1.0_dp+somt0+term0*c0)
              end do
              if(n<2) return
            end if
            m1 = m+1
            !dir$ assume_aligned Phiom:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,n,2
                 t0         = real(i,kind=dp)
                 omt0       = om0*t0
                 somt0      = sin(omt0)
                 tB0        = t0-B2
                 term0      = B2*soma*tB0
                 c0         = soms-t0
                 Phiom(i)   = Phir0*(1.0_dp+somt0+term0*c0)
                 t1         = real(i+1,kind=dp)
                 omt1       = om0*t1
                 somt1      = sin(omt1)
                 tB1        = t1-B2
                 term1      = B2*soma*tB1
                 c1         = soms-t1
                 Phiom(i+1) = Phir0*(1.0_dp+somt1+term1*c1)
                      
            end do
       end subroutine transmitt_hfreq_mod_unroll_2x_r8


       subroutine transmitt_hfreq_mod_r4(Phi0,rho0,om0,Beta,Omega0,Phiom,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmitt_hfreq_mod_r4
           !dir$ attributes forceinline ::   transmitt_hfreq_mod_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmitt_hfreq_mod_r4
           real(kind=sp),                     intent(in) :: Phi0
           real(kind=sp),                     intent(in) :: rho0
           real(kind=sp),                     intent(in) :: om0
           real(kind=sp),                     intent(in) :: Beta
           real(kind=sp),                     intent(in) :: Omega0
           real(kind=sp), dimension(1:n),     intent(out):: Phiom
           integer(kind=i4),                  intent(in) :: n
           real(kind=sp), automatic :: omt0
           real(kind=sp), automatic :: t0
           real(kind=sp), automatic :: somt0
           real(kind=sp), automatic :: tB0
           real(kind=sp), automatic :: term0
           real(kind=sp), automatic :: c0
           real(kind=sp) :: B2,oma,oms,Phiro,soma,soms
           integer(kind=sp) :: i
           B2    = Beta*0.5_sp
           oma   = om0+Omega0
           soma  = sin(oma)
           oms   = om0-Omega0
           soms  = sin(oms)
           Phiro = Phi0*rho0
          
            !dir$ assume_aligned Phiom:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=1,n
                 t0         = real(i,kind=sp)
                 omt0       = om0*t0
                 somt0      = sin(omt0)
                 tB0        = t0-B2
                 term0      = B2*soma*tB0
                 c0         = soms-t0
                 Phiom(i)   = Phir0*(1.0_sp+somt0+term0*c0)
                                          
             end do
       end subroutine transmitt_hfreq_mod_r4


       subroutine transmitt_hfreq_mod_r8(Phi0,rho0,om0,Beta,Omega0,Phiom,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmitt_hfreq_mod_r8
           !dir$ attributes forceinline ::   transmitt_hfreq_mod_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmitt_hfreq_mod_r8
           real(kind=dp),                     intent(in) :: Phi0
           real(kind=dp),                     intent(in) :: rho0
           real(kind=dp),                     intent(in) :: om0
           real(kind=dp),                     intent(in) :: Beta
           real(kind=dp),                     intent(in) :: Omega0
           real(kind=dp), dimension(1:n),     intent(out):: Phiom
           integer(kind=i4),                  intent(in) :: n
           real(kind=dp), automatic :: omt0
           real(kind=dp), automatic :: t0
           real(kind=dp), automatic :: somt0
           real(kind=dp), automatic :: tB0
           real(kind=dp), automatic :: term0
           real(kind=dp), automatic :: c0
           real(kind=dp) :: B2,oma,oms,Phiro,soma,soms
           integer(kind=sp) :: i
           B2    = Beta*0.5_dp
           oma   = om0+Omega0
           soma  = sin(oma)
           oms   = om0-Omega0
           soms  = sin(oms)
           Phiro = Phi0*rho0
          
            !dir$ assume_aligned Phiom:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=1,n
                 t0         = real(i,kind=dp)
                 omt0       = om0*t0
                 somt0      = sin(omt0)
                 tB0        = t0-B2
                 term0      = B2*soma*tB0
                 c0         = soms-t0
                 Phiom(i)   = Phir0*(1.0_dp+somt0+term0*c0)
                                          
             end do
       end subroutine transmitt_hfreq_mod_r8


       subroutine transmitt_hfreq_mod_exec_r4(Phi0,rho0,om0,Beta,Omega0,Phiom,n,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: transmitt_hfreq_mod_exec_r4 
           real(kind=sp),                     intent(in) :: Phi0
           real(kind=sp),                     intent(in) :: rho0
           real(kind=sp),                     intent(in) :: om0
           real(kind=sp),                     intent(in) :: Beta
           real(kind=sp),                     intent(in) :: Omega0
           real(kind=sp), dimension(1:n),     intent(out):: Phiom
           integer(kind=i4),                  intent(in) :: n
           integer(kind=i4),                  intent(in) :: unroll_cnt
           select case (unroll_cnt)
               case (16)
                  call transmitt_hfreq_mod_unroll_16x_r4(Phi0,rho0,om0,Beta,Omega0,Phiom,n)
               case (8)
                  call transmitt_hfreq_mod_unroll_8x_r4(Phi0,rho0,om0,Beta,Omega0,Phiom,n)
               case (4)
                  call transmitt_hfreq_mod_unroll_4x_r4(Phi0,rho0,om0,Beta,Omega0,Phiom,n)
               case (2)
                  call transmitt_hfreq_mod_unroll_2x_r4(Phi0,rho0,om0,Beta,Omega0,Phiom,n)
               case (0)
                  call transmitt_hfreq_mod_r4(Phi0,rho0,om0,Beta,Omega0,Phiom,n)
               case default
                  return
               end select
       end subroutine transmitt_hfreq_mod_exec_r4


       subroutine transmitt_hfreq_mod_exec_r8(Phi0,rho0,om0,Beta,Omega0,Phiom,n,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: transmitt_hfreq_mod_exec_r8 
           real(kind=dp),                     intent(in) :: Phi0
           real(kind=dp),                     intent(in) :: rho0
           real(kind=dp),                     intent(in) :: om0
           real(kind=dp),                     intent(in) :: Beta
           real(kind=dp),                     intent(in) :: Omega0
           real(kind=dp), dimension(1:n),     intent(out):: Phiom
           integer(kind=i4),                  intent(in) :: n
           integer(kind=i4),                  intent(in) :: unroll_cnt
           select case (unroll_cnt)
               case (16)
                  call transmitt_hfreq_mod_unroll_16x_r8(Phi0,rho0,om0,Beta,Omega0,Phiom,n)
               case (8)
                  call transmitt_hfreq_mod_unroll_8x_r8(Phi0,rho0,om0,Beta,Omega0,Phiom,n)
               case (4)
                  call transmitt_hfreq_mod_unroll_4x_r8(Phi0,rho0,om0,Beta,Omega0,Phiom,n)
               case (2)
                  call transmitt_hfreq_mod_unroll_2x_r8(Phi0,rho0,om0,Beta,Omega0,Phiom,n)
               case (0)
                  call transmitt_hfreq_mod_r8(Phi0,rho0,om0,Beta,Omega0,Phiom,n)
               case default
                  return
               end select
       end subroutine transmitt_hfreq_mod_exec_r8

       !Helper subroutine for computing of Bessel j_n values.
       subroutine bessel_jn_beta_r4(bjb,n,Beta)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  bessel_jn_beta_r4
           !dir$ attributes forceinline ::   bessel_jn_beta_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  bessel_jn_beta_r4
           real(kind=sp), dimension(1:n), intent(out) :: bjb
           integer(kind=i4),              intent(in)  :: n
           real(kind=sp),                 intent(in)  :: Beta
           integer(kind=i4) :: i
           !dir$ assume_aligned bjb:64
           do i=1,n
              bjb(i) = bessel_jn(n,Beta)
           end do
       end subroutine bessel_jn_beta_r4


       subroutine bessel_jn_beta_r8(bjb,n,Beta)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  bessel_jn_beta_r8
           !dir$ attributes forceinline ::   bessel_jn_beta_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  bessel_jn_beta_r8
           real(kind=dp), dimension(1:n), intent(out) :: bjb
           integer(kind=i4),              intent(in)  :: n
           real(kind=dp),                 intent(in)  :: Beta
           integer(kind=i4) :: i
           !dir$ assume_aligned bjb:64
           do i=1,n
              bjb(i) = bessel_jn(n,Beta)
           end do
       end subroutine bessel_jn_beta_r8
  
       !Если соотношение р < 1 не выполняется, то исходное 
       !уравнение должно быть применено в общем виде
       !Formula 1, p. 197
       subroutine transmittance_spectr_unroll_16x_r4(rhot,len,bjb,n,rho0,Beta,om0,Omega0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmittance_spectr_unroll_16x_r4
           !dir$ attributes forceinline ::   transmittance_spectr_unroll_16x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmittance_spectr_unroll_16x_r4
           real(kind=sp), dimension(1:len), intent(out) :: rhot
           integer(kind=sp),                intent(in)  :: len
           real(kind=sp), dimension(1:n),   intent(in)  :: bjb
           integer(kind=i4),                intent(in)  :: n
           real(kind=sp),                   intent(in)  :: rho0
           real(kind=sp),                   intent(in)  :: Beta
           real(kind=sp),                   intent(in)  :: om0
           real(kind=sp),                   intent(in)  :: Omega0
           
           real(kind=sp), automatic :: omt0,omt1,omt2,omt3,omt4,omt5,omt6,omt7
           real(kind=sp), automatic :: omt8,omt9,omt10,omt11,omt12,omt13,omt14,omt15
           real(kind=sp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15
           real(kind=sp), automatic :: c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15
           real(kind=sp), automatic :: somt0,somt1,somt2,somt3,somt4,somt5,somt6,somt7
           real(kind=sp), automatic :: somt8,somt9,somt10,somt11,somt12,somt13,somt14,somt15
           real(kind=sp), automatic :: soma0,soma1,soma2,soma3,soma4,soma5,soma6,soma7
           real(kind=sp), automatic :: soma8,soma9,soma10,soma11,soma12,soma13,soma14,soma15
           real(kind=sp), automatic :: soms0,soms1,soms2,soms3,soms4,soms5,soms6,soms7
           real(kind=sp), automatic :: soms8,soms9,soms10,soms11,soms12,soms13,soms14,soms15
           real(kind=sp), automatic :: bj0b,bjn,onep
           integer(kind=i4) :: j,i,m,m1
          
           bj0b = bessel_jn(0,Beta)
           m    = mod(len,16)
           if(m /= 0) then
              do i=1,m
                 c0   = 0.0_sp
                 t0   = real(i,kind=sp)
                 omt0 = om0*t0 
                 do j=1,n
                    soma0 = sin(om0+n*Omega0)*t0
                    soms0 = -1.0**n*sin(om0-n*Omega0)*t0
                    c0    = c0 + bjb(j)*(soma0+soms0)
                 end do
                 somt0 = sin(omt0)
                 rhot(i) = rho0*(1.0_sp+bj0b*somt0+c0)
               end do
               if(len<16) return
            end if
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ assume_aligned bjb:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,len,16
                 c0     = 0.0_sp
                 t0     = real(i,kind=sp)
                 omt0   = om0*t0 
                 somt0  = sin(omt0)
                 c1     = 0.0_sp
                 t1     = real(i+1,kind=sp)
                 omt1   = om0*t1 
                 somt1  = sin(omt1)
                 c2     = 0.0_sp
                 t2     = real(i+2,kind=sp)
                 omt2   = om0*t2 
                 somt2  = sin(omt2)
                 c3     = 0.0_sp
                 t3     = real(i+3,kind=sp)
                 omt3   = om0*t3 
                 somt3  = sin(omt3)
                 c4     = 0.0_sp
                 t4     = real(i+4,kind=sp) 
                 omt4   = om0*t4 
                 somt4  = sin(omt4)
                 c5     = 0.0_sp
                 t5     = real(i+5,kind=sp)
                 omt5   = om0*t5 
                 somt5  = sin(omt5)
                 c6     = 0.0_sp
                 t6     = real(i+6,kind=sp)
                 omt6   = om0*t6 
                 somt6  = sin(omt6)
                 c7     = 0.0_sp
                 t7     = real(i+7,kind=sp)
                 omt7   = om0*t7 
                 somt7  = sin(omt7)
                 c8     = 0.0_sp
                 t8     = real(i+8,kind=sp)
                 omt8   = om0*t8 
                 somt8  = sin(omt8)
                 c9     = 0.0_sp
                 t9     = real(i+9,kind=sp)
                 omt9   = om0*t9 
                 somt9  = sin(omt9)
                 c10    = 0.0_sp
                 t10    = real(i+10,kind=sp)
                 omt10  = om0*t10 
                 somt10 = sin(omt10)
                 c11    = 0.0_sp
                 t11    = real(i+11,kind=sp)
                 omt11  = om0*t11 
                 somt11 = sin(omt11) 
                 c12    = 0.0_sp
                 t12    = real(i+12,kind=sp)
                 omt12  = om0*t12 
                 somt12 = sin(omt12)
                 c13    = 0.0_sp
                 t13    = real(i+13,kind=sp)
                 omt13  = om0*t13
                 somt13 = sin(omt13)
                 c14    = 0.0_sp
                 t14    = real(i+14,kind=sp)
                 omt14  = om0*t14 
                 somt14 = sin(omt14)
                 c15    = 0.0_sp
                 t15    = real(i+15,kind=sp)
                 omt15  = om0*t15 
                 somt15 = sin(omt15)
                 do j=1,n
                    bjn   = bjb(j)
                    onep  = -1.0**n
                    soma0 = sin(om0+n*Omega0)*t0
                    soms0 = onep*sin(om0-n*Omega0)*t0
                    c0    = c0 + bjn*(soma0+soms0)
                    soma1 = sin(om0+n*Omega0)*t1
                    soms1 = onep*sin(om0-n*Omega0)*t1
                    c1    = c1 + bjn*(soma1+soms1)
                    soma2 = sin(om0+n*Omega0)*t2
                    soms2 = onep*sin(om0-n*Omega0)*t2
                    c2    = c2 + bjn*(soma2+soms2)
                    soma3 = sin(om0+n*Omega0)*t3
                    soms3 = onep*sin(om0-n*Omega0)*t3
                    c3    = c3 + bjn*(soma3+soms3)
                    soma4 = sin(om0+n*Omega0)*t4
                    soms4 = onep*sin(om0-n*Omega0)*t4
                    c4    = c4 + bjn*(soma4+soms4)
                    soma5 = sin(om0+n*Omega0)*t5
                    soms5 = onep*sin(om0-n*Omega0)*t5
                    c5    = c5 + bjn*(soma5+soms5)
                    soma6 = sin(om0+n*Omega0)*t6
                    soms6 = onep*sin(om0-n*Omega0)*t6
                    c6    = c6 + bjn*(soma6+soms6)
                    soma7 = sin(om0+n*Omega0)*t7
                    soms7 = onep*sin(om0-n*Omega0)*t7
                    c7    = c7 + bjn*(soma7+soms7)
                    soma8 = sin(om0+n*Omega0)*t8
                    soms8 = onep*sin(om0-n*Omega0)*t8
                    c8    = c8 + bjn*(soma8+soms8)
                    soma9 = sin(om0+n*Omega0)*t9
                    soms9 = onep*sin(om0-n*Omega0)*t9
                    c9    = c9 + bjn*(soma9+soms9)
                    soma10= sin(om0+n*Omega0)*t10
                    soms10= onep*sin(om0-n*Omega0)*t10
                    c10   = c10 + bjn*(soma10+soms10)
                    soma11= sin(om0+n*Omega0)*t11
                    soms11= onep*sin(om0-n*Omega0)*t11
                    c11   = c11 + bjn*(soma11+soms11) 
                    soma12= sin(om0+n*Omega0)*t12
                    soms4 = onep*sin(om0-n*Omega0)*t12
                    c12   = c12 + bjn*(soma12+soms12)
                    soma13= sin(om0+n*Omega0)*t13
                    soms13= onep*sin(om0-n*Omega0)*t13
                    c13   = c13 + bjn*(soma13+soms13)
                    soma14= sin(om0+n*Omega0)*t14
                    soms14= onep*sin(om0-n*Omega0)*t14
                    c14   = c14 + bjn*(soma14+soms14)
                    soma15= sin(om0+n*Omega0)*t15
                    soms15= onep*sin(om0-n*Omega0)*t15
                    c15   = c15 + bjn*(soma15+soms15)
                 end do
                 rhot(i)    = rho0*(1.0_sp+bj0b*somt0+c0)
                 rhot(i+1)  = rho0*(1.0_sp+bj0b*somt1+c1)
                 rhot(i+2)  = rho0*(1.0_sp+bj0b*somt2+c2)
                 rhot(i+3)  = rho0*(1.0_sp+bj0b*somt3+c3)
                 rhot(i+4)  = rho0*(1.0_sp+bj0b*somt4+c4)
                 rhot(i+5)  = rho0*(1.0_sp+bj0b*somt5+c5)
                 rhot(i+6)  = rho0*(1.0_sp+bj0b*somt6+c6)
                 rhot(i+7)  = rho0*(1.0_sp+bj0b*somt7+c7)
                 rhot(i+8)  = rho0*(1.0_sp+bj0b*somt8+c8)
                 rhot(i+9)  = rho0*(1.0_sp+bj0b*somt9+c9)
                 rhot(i+10) = rho0*(1.0_sp+bj0b*somt10+c10)
                 rhot(i+11) = rho0*(1.0_sp+bj0b*somt11+c11)
                 rhot(i+12) = rho0*(1.0_sp+bj0b*somt12+c12)
                 rhot(i+13) = rho0*(1.0_sp+bj0b*somt13+c13)
                 rhot(i+14) = rho0*(1.0_sp+bj0b*somt14+c14)
                 rhot(i+15) = rho0*(1.0_sp+bj0b*somt15+c15)
            end do
       end subroutine transmittance_spectr_unroll_16x_r4


      
       subroutine transmittance_spectr_unroll_16x_r8(rhot,len,bjb,n,rho0,Beta,om0,Omega0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmittance_spectr_unroll_16x_r8
           !dir$ attributes forceinline ::   transmittance_spectr_unroll_16x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmittance_spectr_unroll_16x_r8
           real(kind=dp), dimension(1:len), intent(out) :: rhot
           integer(kind=dp),                intent(in)  :: len
           real(kind=dp), dimension(1:n),   intent(in)  :: bjb
           integer(kind=i4),                intent(in)  :: n
           real(kind=dp),                   intent(in)  :: rho0
           real(kind=dp),                   intent(in)  :: Beta
           real(kind=dp),                   intent(in)  :: om0
           real(kind=dp),                   intent(in)  :: Omega0
           
           real(kind=dp), automatic :: omt0,omt1,omt2,omt3,omt4,omt5,omt6,omt7
           real(kind=dp), automatic :: omt8,omt9,omt10,omt11,omt12,omt13,omt14,omt15
           real(kind=dp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15
           real(kind=dp), automatic :: c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15
           real(kind=dp), automatic :: somt0,somt1,somt2,somt3,somt4,somt5,somt6,somt7
           real(kind=dp), automatic :: somt8,somt9,somt10,somt11,somt12,somt13,somt14,somt15
           real(kind=dp), automatic :: soma0,soma1,soma2,soma3,soma4,soma5,soma6,soma7
           real(kind=dp), automatic :: soma8,soma9,soma10,soma11,soma12,soma13,soma14,soma15
           real(kind=dp), automatic :: soms0,soms1,soms2,soms3,soms4,soms5,soms6,soms7
           real(kind=dp), automatic :: soms8,soms9,soms10,soms11,soms12,soms13,soms14,soms15
           real(kind=dp), automatic :: bj0b,bjn,onep
           integer(kind=i4) :: j,i,m,m1
          
           bj0b = bessel_jn(0,Beta)
           m    = mod(len,16)
           if(m /= 0) then
              do i=1,m
                 c0   = 0.0_dp
                 t0   = real(i,kind=sp)
                 omt0 = om0*t0 
                 do j=1,n
                    soma0 = sin(om0+n*Omega0)*t0
                    soms0 = -1.0_dp**n*sin(om0-n*Omega0)*t0
                    c0    = c0 + bjb(j)*(soma0+soms0)
                 end do
                 somt0 = sin(omt0)
                 rhot(i) = rho0*(1.0_dp+bj0b*somt0+c0)
               end do
               if(len<16) return
            end if
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ assume_aligned bjb:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,len,16
                 c0     = 0.0_dp
                 t0     = real(i,kind=dp)
                 omt0   = om0*t0 
                 somt0  = sin(omt0)
                 c1     = 0.0_dp
                 t1     = real(i+1,kind=dp)
                 omt1   = om0*t1 
                 somt1  = sin(omt1)
                 c2     = 0.0_dp
                 t2     = real(i+2,kind=dp)
                 omt2   = om0*t2 
                 somt2  = sin(omt2)
                 c3     = 0.0_dp
                 t3     = real(i+3,kind=dp)
                 omt3   = om0*t3 
                 somt3  = sin(omt3)
                 c4     = 0.0_dp
                 t4     = real(i+4,kind=dp) 
                 omt4   = om0*t4 
                 somt4  = sin(omt4)
                 c5     = 0.0_dp
                 t5     = real(i+5,kind=dp)
                 omt5   = om0*t5 
                 somt5  = sin(omt5)
                 c6     = 0.0_dp
                 t6     = real(i+6,kind=dp)
                 omt6   = om0*t6 
                 somt6  = sin(omt6)
                 c7     = 0.0_dp
                 t7     = real(i+7,kind=dp)
                 omt7   = om0*t7 
                 somt7  = sin(omt7)
                 c8     = 0.0_dp
                 t8     = real(i+8,kind=dp)
                 omt8   = om0*t8 
                 somt8  = sin(omt8)
                 c9     = 0.0_dp
                 t9     = real(i+9,kind=dp)
                 omt9   = om0*t9 
                 somt9  = sin(omt9)
                 c10    = 0.0_dp
                 t10    = real(i+10,kind=dp)
                 omt10  = om0*t10 
                 somt10 = sin(omt10)
                 c11    = 0.0_dp
                 t11    = real(i+11,kind=dp)
                 omt11  = om0*t11 
                 somt11 = sin(omt11) 
                 c12    = 0.0_dp
                 t12    = real(i+12,kind=dp)
                 omt12  = om0*t12 
                 somt12 = sin(omt12)
                 c13    = 0.0_dp
                 t13    = real(i+13,kind=dp)
                 omt13  = om0*t13
                 somt13 = sin(omt13)
                 c14    = 0.0_dp
                 t14    = real(i+14,kind=dp)
                 omt14  = om0*t14 
                 somt14 = sin(omt14)
                 c15    = 0.0_dp
                 t15    = real(i+15,kind=dp)
                 omt15  = om0*t15 
                 somt15 = sin(omt15)
                 do j=1,n
                    bjn   = bjb(j)
                    onep  = -1.0**n
                    soma0 = sin(om0+n*Omega0)*t0
                    soms0 = onep*sin(om0-n*Omega0)*t0
                    c0    = c0 + bjn*(soma0+soms0)
                    soma1 = sin(om0+n*Omega0)*t1
                    soms1 = onep*sin(om0-n*Omega0)*t1
                    c1    = c1 + bjn*(soma1+soms1)
                    soma2 = sin(om0+n*Omega0)*t2
                    soms2 = onep*sin(om0-n*Omega0)*t2
                    c2    = c2 + bjn*(soma2+soms2)
                    soma3 = sin(om0+n*Omega0)*t3
                    soms3 = onep*sin(om0-n*Omega0)*t3
                    c3    = c3 + bjn*(soma3+soms3)
                    soma4 = sin(om0+n*Omega0)*t4
                    soms4 = onep*sin(om0-n*Omega0)*t4
                    c4    = c4 + bjn*(soma4+soms4)
                    soma5 = sin(om0+n*Omega0)*t5
                    soms5 = onep*sin(om0-n*Omega0)*t5
                    c5    = c5 + bjn*(soma5+soms5)
                    soma6 = sin(om0+n*Omega0)*t6
                    soms6 = onep*sin(om0-n*Omega0)*t6
                    c6    = c6 + bjn*(soma6+soms6)
                    soma7 = sin(om0+n*Omega0)*t7
                    soms7 = onep*sin(om0-n*Omega0)*t7
                    c7    = c7 + bjn*(soma7+soms7)
                    soma8 = sin(om0+n*Omega0)*t8
                    soms8 = onep*sin(om0-n*Omega0)*t8
                    c8    = c8 + bjn*(soma8+soms8)
                    soma9 = sin(om0+n*Omega0)*t9
                    soms9 = onep*sin(om0-n*Omega0)*t9
                    c9    = c9 + bjn*(soma9+soms9)
                    soma10= sin(om0+n*Omega0)*t10
                    soms10= onep*sin(om0-n*Omega0)*t10
                    c10   = c10 + bjn*(soma10+soms10)
                    soma11= sin(om0+n*Omega0)*t11
                    soms11= onep*sin(om0-n*Omega0)*t11
                    c11   = c11 + bjn*(soma11+soms11) 
                    soma12= sin(om0+n*Omega0)*t12
                    soms4 = onep*sin(om0-n*Omega0)*t12
                    c12   = c12 + bjn*(soma12+soms12)
                    soma13= sin(om0+n*Omega0)*t13
                    soms13= onep*sin(om0-n*Omega0)*t13
                    c13   = c13 + bjn*(soma13+soms13)
                    soma14= sin(om0+n*Omega0)*t14
                    soms14= onep*sin(om0-n*Omega0)*t14
                    c14   = c14 + bjn*(soma14+soms14)
                    soma15= sin(om0+n*Omega0)*t15
                    soms15= onep*sin(om0-n*Omega0)*t15
                    c15   = c15 + bjn*(soma15+soms15)
                 end do
                 rhot(i)    = rho0*(1.0_dp+bj0b*somt0+c0)
                 rhot(i+1)  = rho0*(1.0_dp+bj0b*somt1+c1)
                 rhot(i+2)  = rho0*(1.0_dp+bj0b*somt2+c2)
                 rhot(i+3)  = rho0*(1.0_dp+bj0b*somt3+c3)
                 rhot(i+4)  = rho0*(1.0_dp+bj0b*somt4+c4)
                 rhot(i+5)  = rho0*(1.0_dp+bj0b*somt5+c5)
                 rhot(i+6)  = rho0*(1.0_dp+bj0b*somt6+c6)
                 rhot(i+7)  = rho0*(1.0_dp+bj0b*somt7+c7)
                 rhot(i+8)  = rho0*(1.0_dp+bj0b*somt8+c8)
                 rhot(i+9)  = rho0*(1.0_dp+bj0b*somt9+c9)
                 rhot(i+10) = rho0*(1.0_dp+bj0b*somt10+c10)
                 rhot(i+11) = rho0*(1.0_dp+bj0b*somt11+c11)
                 rhot(i+12) = rho0*(1.0_dp+bj0b*somt12+c12)
                 rhot(i+13) = rho0*(1.0_dp+bj0b*somt13+c13)
                 rhot(i+14) = rho0*(1.0_dp+bj0b*somt14+c14)
                 rhot(i+15) = rho0*(1.0_dp+bj0b*somt15+c15)
            end do
       end subroutine transmittance_spectr_unroll_16x_r8



       subroutine transmittance_spectr_unroll_8x_r4(rhot,len,bjb,n,rho0,Beta,om0,Omega0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmittance_spectr_unroll_8x_r4
           !dir$ attributes forceinline ::   transmittance_spectr_unroll_8x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmittance_spectr_unroll_8x_r4
           real(kind=sp), dimension(1:len), intent(out) :: rhot
           integer(kind=sp),                intent(in)  :: len
           real(kind=sp), dimension(1:n),   intent(in)  :: bjb
           integer(kind=i4),                intent(in)  :: n
           real(kind=sp),                   intent(in)  :: rho0
           real(kind=sp),                   intent(in)  :: Beta
           real(kind=sp),                   intent(in)  :: om0
           real(kind=sp),                   intent(in)  :: Omega0
           
           real(kind=sp), automatic :: omt0,omt1,omt2,omt3,omt4,omt5,omt6,omt7
           real(kind=sp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7,t8
           real(kind=sp), automatic :: c0,c1,c2,c3,c4,c5,c6,c7
           real(kind=sp), automatic :: somt0,somt1,somt2,somt3,somt4,somt5,somt6,somt7
           real(kind=sp), automatic :: soma0,soma1,soma2,soma3,soma4,soma5,soma6,soma7
           real(kind=sp), automatic :: soms0,soms1,soms2,soms3,soms4,soms5,soms6,soms7
           real(kind=sp), automatic :: bj0b,bjn,onep
           integer(kind=i4) :: j,i,m,m1
          
           bj0b = bessel_jn(0,Beta)
           m    = mod(len,8)
           if(m /= 0) then
              do i=1,m
                 c0   = 0.0_sp
                 t0   = real(i,kind=sp)
                 omt0 = om0*t0 
                 do j=1,n
                    soma0 = sin(om0+n*Omega0)*t0
                    soms0 = -1.0**n*sin(om0-n*Omega0)*t0
                    c0    = c0 + bjb(j)*(soma0+soms0)
                 end do
                 somt0 = sin(omt0)
                 rhot(i) = rho0*(1.0_sp+bj0b*somt0+c0)
               end do
               if(len<8) return
            end if
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ assume_aligned bjb:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,len,8
                 c0     = 0.0_sp
                 t0     = real(i,kind=sp)
                 omt0   = om0*t0 
                 somt0  = sin(omt0)
                 c1     = 0.0_sp
                 t1     = real(i+1,kind=sp)
                 omt1   = om0*t1 
                 somt1  = sin(omt1)
                 c2     = 0.0_sp
                 t2     = real(i+2,kind=sp)
                 omt2   = om0*t2 
                 somt2  = sin(omt2)
                 c3     = 0.0_sp
                 t3     = real(i+3,kind=sp)
                 omt3   = om0*t3 
                 somt3  = sin(omt3)
                 c4     = 0.0_sp
                 t4     = real(i+4,kind=sp) 
                 omt4   = om0*t4 
                 somt4  = sin(omt4)
                 c5     = 0.0_sp
                 t5     = real(i+5,kind=sp)
                 omt5   = om0*t5 
                 somt5  = sin(omt5)
                 c6     = 0.0_sp
                 t6     = real(i+6,kind=sp)
                 omt6   = om0*t6 
                 somt6  = sin(omt6)
                 c7     = 0.0_sp
                 t7     = real(i+7,kind=sp)
                 omt7   = om0*t7 
                 somt7  = sin(omt7)
                 do j=1,n
                    bjn   = bjb(j)
                    onep  = -1.0**n
                    soma0 = sin(om0+n*Omega0)*t0
                    soms0 = onep*sin(om0-n*Omega0)*t0
                    c0    = c0 + bjn*(soma0+soms0)
                    soma1 = sin(om0+n*Omega0)*t1
                    soms1 = onep*sin(om0-n*Omega0)*t1
                    c1    = c1 + bjn*(soma1+soms1)
                    soma2 = sin(om0+n*Omega0)*t2
                    soms2 = onep*sin(om0-n*Omega0)*t2
                    c2    = c2 + bjn*(soma2+soms2)
                    soma3 = sin(om0+n*Omega0)*t3
                    soms3 = onep*sin(om0-n*Omega0)*t3
                    c3    = c3 + bjn*(soma3+soms3)
                    soma4 = sin(om0+n*Omega0)*t4
                    soms4 = onep*sin(om0-n*Omega0)*t4
                    c4    = c4 + bjn*(soma4+soms4)
                    soma5 = sin(om0+n*Omega0)*t5
                    soms5 = onep*sin(om0-n*Omega0)*t5
                    c5    = c5 + bjn*(soma5+soms5)
                    soma6 = sin(om0+n*Omega0)*t6
                    soms6 = onep*sin(om0-n*Omega0)*t6
                    c6    = c6 + bjn*(soma6+soms6)
                    soma7 = sin(om0+n*Omega0)*t7
                    soms7 = onep*sin(om0-n*Omega0)*t7
                    c7    = c7 + bjn*(soma7+soms7)
                 end do
                 rhot(i)    = rho0*(1.0_sp+bj0b*somt0+c0)
                 rhot(i+1)  = rho0*(1.0_sp+bj0b*somt1+c1)
                 rhot(i+2)  = rho0*(1.0_sp+bj0b*somt2+c2)
                 rhot(i+3)  = rho0*(1.0_sp+bj0b*somt3+c3)
                 rhot(i+4)  = rho0*(1.0_sp+bj0b*somt4+c4)
                 rhot(i+5)  = rho0*(1.0_sp+bj0b*somt5+c5)
                 rhot(i+6)  = rho0*(1.0_sp+bj0b*somt6+c6)
                 rhot(i+7)  = rho0*(1.0_sp+bj0b*somt7+c7)
             end do
       end subroutine transmittance_spectr_unroll_8x_r4


  
       subroutine transmittance_spectr_unroll_8x_r8(rhot,len,bjb,n,rho0,Beta,om0,Omega0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmittance_spectr_unroll_8x_r8
           !dir$ attributes forceinline ::   transmittance_spectr_unroll_8x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmittance_spectr_unroll_8x_r8
           real(kind=dp), dimension(1:len), intent(out) :: rhot
           integer(kind=dp),                intent(in)  :: len
           real(kind=dp), dimension(1:n),   intent(in)  :: bjb
           integer(kind=i4),                intent(in)  :: n
           real(kind=dp),                   intent(in)  :: rho0
           real(kind=dp),                   intent(in)  :: Beta
           real(kind=dp),                   intent(in)  :: om0
           real(kind=dp),                   intent(in)  :: Omega0
           
           real(kind=dp), automatic :: omt0,omt1,omt2,omt3,omt4,omt5,omt6,omt7
           real(kind=dp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=dp), automatic :: c0,c1,c2,c3,c4,c5,c6,c7
           real(kind=dp), automatic :: somt0,somt1,somt2,somt3,somt4,somt5,somt6,somt7
           real(kind=dp), automatic :: soma0,soma1,soma2,soma3,soma4,soma5,soma6,soma7
           real(kind=dp), automatic :: soms0,soms1,soms2,soms3,soms4,soms5,soms6,soms7
           real(kind=dp), automatic :: bj0b,bjn,onep
           integer(kind=i4) :: j,i,m,m1
          
           bj0b = bessel_jn(0,Beta)
           m    = mod(len,8)
           if(m /= 0) then
              do i=1,m
                 c0   = 0.0_dp
                 t0   = real(i,kind=sp)
                 omt0 = om0*t0 
                 do j=1,n
                    soma0 = sin(om0+n*Omega0)*t0
                    soms0 = -1.0_dp**n*sin(om0-n*Omega0)*t0
                    c0    = c0 + bjb(j)*(soma0+soms0)
                 end do
                 somt0 = sin(omt0)
                 rhot(i) = rho0*(1.0_dp+bj0b*somt0+c0)
               end do
               if(len<8) return
            end if
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ assume_aligned bjb:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,len,8
                 c0     = 0.0_dp
                 t0     = real(i,kind=dp)
                 omt0   = om0*t0 
                 somt0  = sin(omt0)
                 c1     = 0.0_dp
                 t1     = real(i+1,kind=dp)
                 omt1   = om0*t1 
                 somt1  = sin(omt1)
                 c2     = 0.0_dp
                 t2     = real(i+2,kind=dp)
                 omt2   = om0*t2 
                 somt2  = sin(omt2)
                 c3     = 0.0_dp
                 t3     = real(i+3,kind=dp)
                 omt3   = om0*t3 
                 somt3  = sin(omt3)
                 c4     = 0.0_dp
                 t4     = real(i+4,kind=dp) 
                 omt4   = om0*t4 
                 somt4  = sin(omt4)
                 c5     = 0.0_dp
                 t5     = real(i+5,kind=dp)
                 omt5   = om0*t5 
                 somt5  = sin(omt5)
                 c6     = 0.0_dp
                 t6     = real(i+6,kind=dp)
                 omt6   = om0*t6 
                 somt6  = sin(omt6)
                 c7     = 0.0_dp
                 t7     = real(i+7,kind=dp)
                 omt7   = om0*t7 
                 somt7  = sin(omt7)
                 do j=1,n
                    bjn   = bjb(j)
                    onep  = -1.0**n
                    soma0 = sin(om0+n*Omega0)*t0
                    soms0 = onep*sin(om0-n*Omega0)*t0
                    c0    = c0 + bjn*(soma0+soms0)
                    soma1 = sin(om0+n*Omega0)*t1
                    soms1 = onep*sin(om0-n*Omega0)*t1
                    c1    = c1 + bjn*(soma1+soms1)
                    soma2 = sin(om0+n*Omega0)*t2
                    soms2 = onep*sin(om0-n*Omega0)*t2
                    c2    = c2 + bjn*(soma2+soms2)
                    soma3 = sin(om0+n*Omega0)*t3
                    soms3 = onep*sin(om0-n*Omega0)*t3
                    c3    = c3 + bjn*(soma3+soms3)
                    soma4 = sin(om0+n*Omega0)*t4
                    soms4 = onep*sin(om0-n*Omega0)*t4
                    c4    = c4 + bjn*(soma4+soms4)
                    soma5 = sin(om0+n*Omega0)*t5
                    soms5 = onep*sin(om0-n*Omega0)*t5
                    c5    = c5 + bjn*(soma5+soms5)
                    soma6 = sin(om0+n*Omega0)*t6
                    soms6 = onep*sin(om0-n*Omega0)*t6
                    c6    = c6 + bjn*(soma6+soms6)
                    soma7 = sin(om0+n*Omega0)*t7
                    soms7 = onep*sin(om0-n*Omega0)*t7
                    c7    = c7 + bjn*(soma7+soms7)
                end do
                 rhot(i)    = rho0*(1.0_dp+bj0b*somt0+c0)
                 rhot(i+1)  = rho0*(1.0_dp+bj0b*somt1+c1)
                 rhot(i+2)  = rho0*(1.0_dp+bj0b*somt2+c2)
                 rhot(i+3)  = rho0*(1.0_dp+bj0b*somt3+c3)
                 rhot(i+4)  = rho0*(1.0_dp+bj0b*somt4+c4)
                 rhot(i+5)  = rho0*(1.0_dp+bj0b*somt5+c5)
                 rhot(i+6)  = rho0*(1.0_dp+bj0b*somt6+c6)
                 rhot(i+7)  = rho0*(1.0_dp+bj0b*somt7+c7)
             end do
       end subroutine transmittance_spectr_unroll_8x_r8



       subroutine transmittance_spectr_unroll_4x_r4(rhot,len,bjb,n,rho0,Beta,om0,Omega0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmittance_spectr_unroll_4x_r4
           !dir$ attributes forceinline ::   transmittance_spectr_unroll_4x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmittance_spectr_unroll_4x_r4
           real(kind=sp), dimension(1:len), intent(out) :: rhot
           integer(kind=sp),                intent(in)  :: len
           real(kind=sp), dimension(1:n),   intent(in)  :: bjb
           integer(kind=i4),                intent(in)  :: n
           real(kind=sp),                   intent(in)  :: rho0
           real(kind=sp),                   intent(in)  :: Beta
           real(kind=sp),                   intent(in)  :: om0
           real(kind=sp),                   intent(in)  :: Omega0
           
           real(kind=sp), automatic :: omt0,omt1,omt2,omt3
           real(kind=sp), automatic :: t0,t1,t2,t3
           real(kind=sp), automatic :: c0,c1,c2,c3
           real(kind=sp), automatic :: somt0,somt1,somt2,somt3
           real(kind=sp), automatic :: soma0,soma1,soma2,soma3
           real(kind=sp), automatic :: soms0,soms1,soms2,soms3
           real(kind=sp), automatic :: bj0b,bjn,onep
           integer(kind=i4) :: j,i,m,m1
          
           bj0b = bessel_jn(0,Beta)
           m    = mod(len,4)
           if(m /= 0) then
              do i=1,m
                 c0   = 0.0_sp
                 t0   = real(i,kind=sp)
                 omt0 = om0*t0 
                 do j=1,n
                    soma0 = sin(om0+n*Omega0)*t0
                    soms0 = -1.0**n*sin(om0-n*Omega0)*t0
                    c0    = c0 + bjb(j)*(soma0+soms0)
                 end do
                 somt0 = sin(omt0)
                 rhot(i) = rho0*(1.0_sp+bj0b*somt0+c0)
               end do
               if(len<4) return
            end if
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ assume_aligned bjb:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,len,4
                 c0     = 0.0_sp
                 t0     = real(i,kind=sp)
                 omt0   = om0*t0 
                 somt0  = sin(omt0)
                 c1     = 0.0_sp
                 t1     = real(i+1,kind=sp)
                 omt1   = om0*t1 
                 somt1  = sin(omt1)
                 c2     = 0.0_sp
                 t2     = real(i+2,kind=sp)
                 omt2   = om0*t2 
                 somt2  = sin(omt2)
                 c3     = 0.0_sp
                 t3     = real(i+3,kind=sp)
                 omt3   = om0*t3 
                 somt3  = sin(omt3)
                 do j=1,n
                    bjn   = bjb(j)
                    onep  = -1.0**n
                    soma0 = sin(om0+n*Omega0)*t0
                    soms0 = onep*sin(om0-n*Omega0)*t0
                    c0    = c0 + bjn*(soma0+soms0)
                    soma1 = sin(om0+n*Omega0)*t1
                    soms1 = onep*sin(om0-n*Omega0)*t1
                    c1    = c1 + bjn*(soma1+soms1)
                    soma2 = sin(om0+n*Omega0)*t2
                    soms2 = onep*sin(om0-n*Omega0)*t2
                    c2    = c2 + bjn*(soma2+soms2)
                    soma3 = sin(om0+n*Omega0)*t3
                    soms3 = onep*sin(om0-n*Omega0)*t3
                    c3    = c3 + bjn*(soma3+soms3)
                 end do
                 rhot(i)    = rho0*(1.0_sp+bj0b*somt0+c0)
                 rhot(i+1)  = rho0*(1.0_sp+bj0b*somt1+c1)
                 rhot(i+2)  = rho0*(1.0_sp+bj0b*somt2+c2)
                 rhot(i+3)  = rho0*(1.0_sp+bj0b*somt3+c3)
                
             end do
       end subroutine transmittance_spectr_unroll_4x_r4


       subroutine transmittance_spectr_unroll_4x_r8(rhot,len,bjb,n,rho0,Beta,om0,Omega0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmittance_spectr_unroll_4x_r8
           !dir$ attributes forceinline ::   transmittance_spectr_unroll_4x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmittance_spectr_unroll_4x_r8
           real(kind=dp), dimension(1:len), intent(out) :: rhot
           integer(kind=dp),                intent(in)  :: len
           real(kind=dp), dimension(1:n),   intent(in)  :: bjb
           integer(kind=i4),                intent(in)  :: n
           real(kind=dp),                   intent(in)  :: rho0
           real(kind=dp),                   intent(in)  :: Beta
           real(kind=dp),                   intent(in)  :: om0
           real(kind=dp),                   intent(in)  :: Omega0
           
           real(kind=dp), automatic :: omt0,omt1,omt2,omt3
           real(kind=dp), automatic :: t0,t1,t2,t3
           real(kind=dp), automatic :: c0,c1,c2,c3
           real(kind=dp), automatic :: somt0,somt1,somt2,somt3
           real(kind=dp), automatic :: soma0,soma1,soma2,soma3
           real(kind=dp), automatic :: soms0,soms1,soms2,soms3
           real(kind=dp), automatic :: bj0b,bjn,onep
           integer(kind=i4) :: j,i,m,m1
          
           bj0b = bessel_jn(0,Beta)
           m    = mod(len,4)
           if(m /= 0) then
              do i=1,m
                 c0   = 0.0_dp
                 t0   = real(i,kind=sp)
                 omt0 = om0*t0 
                 do j=1,n
                    soma0 = sin(om0+n*Omega0)*t0
                    soms0 = -1.0_dp**n*sin(om0-n*Omega0)*t0
                    c0    = c0 + bjb(j)*(soma0+soms0)
                 end do
                 somt0 = sin(omt0)
                 rhot(i) = rho0*(1.0_dp+bj0b*somt0+c0)
               end do
               if(len<4) return
            end if
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ assume_aligned bjb:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,len,4
                 c0     = 0.0_dp
                 t0     = real(i,kind=dp)
                 omt0   = om0*t0 
                 somt0  = sin(omt0)
                 c1     = 0.0_dp
                 t1     = real(i+1,kind=dp)
                 omt1   = om0*t1 
                 somt1  = sin(omt1)
                 c2     = 0.0_dp
                 t2     = real(i+2,kind=dp)
                 omt2   = om0*t2 
                 somt2  = sin(omt2)
                 c3     = 0.0_dp
                 t3     = real(i+3,kind=dp)
                 omt3   = om0*t3 
                 somt3  = sin(omt3)
                 do j=1,n
                    bjn   = bjb(j)
                    onep  = -1.0**n
                    soma0 = sin(om0+n*Omega0)*t0
                    soms0 = onep*sin(om0-n*Omega0)*t0
                    c0    = c0 + bjn*(soma0+soms0)
                    soma1 = sin(om0+n*Omega0)*t1
                    soms1 = onep*sin(om0-n*Omega0)*t1
                    c1    = c1 + bjn*(soma1+soms1)
                    soma2 = sin(om0+n*Omega0)*t2
                    soms2 = onep*sin(om0-n*Omega0)*t2
                    c2    = c2 + bjn*(soma2+soms2)
                    soma3 = sin(om0+n*Omega0)*t3
                    soms3 = onep*sin(om0-n*Omega0)*t3
                    c3    = c3 + bjn*(soma3+soms3)
                 end do
                 rhot(i)    = rho0*(1.0_dp+bj0b*somt0+c0)
                 rhot(i+1)  = rho0*(1.0_dp+bj0b*somt1+c1)
                 rhot(i+2)  = rho0*(1.0_dp+bj0b*somt2+c2)
                 rhot(i+3)  = rho0*(1.0_dp+bj0b*somt3+c3)
             end do
       end subroutine transmittance_spectr_unroll_4x_r8


       subroutine transmittance_spectr_unroll_2x_r4(rhot,len,bjb,n,rho0,Beta,om0,Omega0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmittance_spectr_unroll_2x_r4
           !dir$ attributes forceinline ::   transmittance_spectr_unroll_2x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmittance_spectr_unroll_2x_r4
           real(kind=sp), dimension(1:len), intent(out) :: rhot
           integer(kind=sp),                intent(in)  :: len
           real(kind=sp), dimension(1:n),   intent(in)  :: bjb
           integer(kind=i4),                intent(in)  :: n
           real(kind=sp),                   intent(in)  :: rho0
           real(kind=sp),                   intent(in)  :: Beta
           real(kind=sp),                   intent(in)  :: om0
           real(kind=sp),                   intent(in)  :: Omega0
           
           real(kind=sp), automatic :: omt0,omt1
           real(kind=sp), automatic :: t0,t1
           real(kind=sp), automatic :: c0,c1
           real(kind=sp), automatic :: somt0,somt1
           real(kind=sp), automatic :: soma0,soma1
           real(kind=sp), automatic :: soms0,soms1
           real(kind=sp), automatic :: bj0b,bjn,onep
           integer(kind=i4) :: j,i,m,m1
          
           bj0b = bessel_jn(0,Beta)
           m    = mod(len,2)
           if(m /= 0) then
              do i=1,m
                 c0   = 0.0_sp
                 t0   = real(i,kind=sp)
                 omt0 = om0*t0 
                 do j=1,n
                    soma0 = sin(om0+n*Omega0)*t0
                    soms0 = -1.0**n*sin(om0-n*Omega0)*t0
                    c0    = c0 + bjb(j)*(soma0+soms0)
                 end do
                 somt0 = sin(omt0)
                 rhot(i) = rho0*(1.0_sp+bj0b*somt0+c0)
               end do
               if(len<2) return
            end if
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ assume_aligned bjb:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,len,2
                 c0     = 0.0_sp
                 t0     = real(i,kind=sp)
                 omt0   = om0*t0 
                 somt0  = sin(omt0)
                 c1     = 0.0_sp
                 t1     = real(i+1,kind=sp)
                 omt1   = om0*t1 
                 somt1  = sin(omt1)
                 do j=1,n
                    bjn   = bjb(j)
                    onep  = -1.0**n
                    soma0 = sin(om0+n*Omega0)*t0
                    soms0 = onep*sin(om0-n*Omega0)*t0
                    c0    = c0 + bjn*(soma0+soms0)
                    soma1 = sin(om0+n*Omega0)*t1
                    soms1 = onep*sin(om0-n*Omega0)*t1
                    c1    = c1 + bjn*(soma1+soms1)
                 end do
                 rhot(i)    = rho0*(1.0_sp+bj0b*somt0+c0)
                 rhot(i+1)  = rho0*(1.0_sp+bj0b*somt1+c1)
                               
             end do
       end subroutine transmittance_spectr_unroll_2x_r4


       subroutine transmittance_spectr_unroll_2x_r8(rhot,len,bjb,n,rho0,Beta,om0,Omega0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmittance_spectr_unroll_2x_r8
           !dir$ attributes forceinline ::   transmittance_spectr_unroll_2x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmittance_spectr_unroll_2x_r8
           real(kind=dp), dimension(1:len), intent(out) :: rhot
           integer(kind=dp),                intent(in)  :: len
           real(kind=dp), dimension(1:n),   intent(in)  :: bjb
           integer(kind=i4),                intent(in)  :: n
           real(kind=dp),                   intent(in)  :: rho0
           real(kind=dp),                   intent(in)  :: Beta
           real(kind=dp),                   intent(in)  :: om0
           real(kind=dp),                   intent(in)  :: Omega0
           
           real(kind=dp), automatic :: omt0,omt1
           real(kind=dp), automatic :: t0,t1
           real(kind=dp), automatic :: c0,c1
           real(kind=dp), automatic :: somt0,somt1
           real(kind=dp), automatic :: soma0,soma1
           real(kind=dp), automatic :: soms0,soms1
           real(kind=dp), automatic :: bj0b,bjn,onep
           integer(kind=i4) :: j,i,m,m1
          
           bj0b = bessel_jn(0,Beta)
           m    = mod(len,2)
           if(m /= 0) then
              do i=1,m
                 c0   = 0.0_dp
                 t0   = real(i,kind=sp)
                 omt0 = om0*t0 
                 do j=1,n
                    soma0 = sin(om0+n*Omega0)*t0
                    soms0 = -1.0_dp**n*sin(om0-n*Omega0)*t0
                    c0    = c0 + bjb(j)*(soma0+soms0)
                 end do
                 somt0 = sin(omt0)
                 rhot(i) = rho0*(1.0_dp+bj0b*somt0+c0)
               end do
               if(len<2) return
            end if
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ assume_aligned bjb:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,len,2
                 c0     = 0.0_dp
                 t0     = real(i,kind=dp)
                 omt0   = om0*t0 
                 somt0  = sin(omt0)
                 c1     = 0.0_dp
                 t1     = real(i+1,kind=dp)
                 omt1   = om0*t1 
                 somt1  = sin(omt1)
                 do j=1,n
                    bjn   = bjb(j)
                    onep  = -1.0**n
                    soma0 = sin(om0+n*Omega0)*t0
                    soms0 = onep*sin(om0-n*Omega0)*t0
                    c0    = c0 + bjn*(soma0+soms0)
                    soma1 = sin(om0+n*Omega0)*t1
                    soms1 = onep*sin(om0-n*Omega0)*t1
                    c1    = c1 + bjn*(soma1+soms1)
                 end do
                 rhot(i)    = rho0*(1.0_dp+bj0b*somt0+c0)
                 rhot(i+1)  = rho0*(1.0_dp+bj0b*somt1+c1)
                
             end do
       end subroutine transmittance_spectr_unroll_2x_r8


       subroutine transmittance_spectr_unroll_2x_r8(rhot,len,bjb,n,rho0,Beta,om0,Omega0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmittance_spectr_unroll_2x_r8
           !dir$ attributes forceinline ::   transmittance_spectr_unroll_2x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmittance_spectr_unroll_2x_r8
           real(kind=dp), dimension(1:len), intent(out) :: rhot
           integer(kind=dp),                intent(in)  :: len
           real(kind=dp), dimension(1:n),   intent(in)  :: bjb
           integer(kind=i4),                intent(in)  :: n
           real(kind=dp),                   intent(in)  :: rho0
           real(kind=dp),                   intent(in)  :: Beta
           real(kind=dp),                   intent(in)  :: om0
           real(kind=dp),                   intent(in)  :: Omega0
           
           real(kind=dp), automatic :: omt0,omt1
           real(kind=dp), automatic :: t0,t1
           real(kind=dp), automatic :: c0,c1
           real(kind=dp), automatic :: somt0,somt1
           real(kind=dp), automatic :: soma0,soma1
           real(kind=dp), automatic :: soms0,soms1
           real(kind=dp), automatic :: bj0b,bjn,onep
           integer(kind=i4) :: j,i,m,m1
          
           bj0b = bessel_jn(0,Beta)
           m    = mod(len,2)
           if(m /= 0) then
              do i=1,m
                 c0   = 0.0_dp
                 t0   = real(i,kind=sp)
                 omt0 = om0*t0 
                 do j=1,n
                    soma0 = sin(om0+n*Omega0)*t0
                    soms0 = -1.0_dp**n*sin(om0-n*Omega0)*t0
                    c0    = c0 + bjb(j)*(soma0+soms0)
                 end do
                 somt0 = sin(omt0)
                 rhot(i) = rho0*(1.0_dp+bj0b*somt0+c0)
               end do
               if(len<2) return
            end if
            m1 = m+1
            !dir$ assume_aligned rhot:64
            !dir$ assume_aligned bjb:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=m1,len,2
                 c0     = 0.0_dp
                 t0     = real(i,kind=dp)
                 omt0   = om0*t0 
                 somt0  = sin(omt0)
                 c1     = 0.0_dp
                 t1     = real(i+1,kind=dp)
                 omt1   = om0*t1 
                 somt1  = sin(omt1)
                 do j=1,n
                    bjn   = bjb(j)
                    onep  = -1.0**n
                    soma0 = sin(om0+n*Omega0)*t0
                    soms0 = onep*sin(om0-n*Omega0)*t0
                    c0    = c0 + bjn*(soma0+soms0)
                    soma1 = sin(om0+n*Omega0)*t1
                    soms1 = onep*sin(om0-n*Omega0)*t1
                    c1    = c1 + bjn*(soma1+soms1)
                 end do
                 rhot(i)    = rho0*(1.0_dp+bj0b*somt0+c0)
                 rhot(i+1)  = rho0*(1.0_dp+bj0b*somt1+c1)
                
             end do
       end subroutine transmittance_spectr_unroll_2x_r8


       subroutine transmittance_spectr_r4(rhot,len,bjb,n,rho0,Beta,om0,Omega0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmittance_spectr_r4
           !dir$ attributes forceinline ::   transmittance_spectr_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmittance_spectr_r4
           real(kind=sp), dimension(1:len), intent(out) :: rhot
           integer(kind=i4),                intent(in)  :: len
           real(kind=sp), dimension(1:n),   intent(in)  :: bjb
           integer(kind=i4),                intent(in)  :: n
           real(kind=sp),                   intent(in)  :: rho0
           real(kind=sp),                   intent(in)  :: Beta
           real(kind=sp),                   intent(in)  :: om0
           real(kind=sp),                   intent(in)  :: Omega0
           
           real(kind=sp), automatic :: omt0
           real(kind=sp), automatic :: t0
           real(kind=sp), automatic :: c0
           real(kind=sp), automatic :: somt0
           real(kind=sp), automatic :: soma0
           real(kind=sp), automatic :: soms0
           real(kind=sp), automatic :: bj0b,bjn,onep
           integer(kind=i4) :: j,i
          
           bj0b = bessel_jn(0,Beta)
          
            !dir$ assume_aligned rhot:64
            !dir$ assume_aligned bjb:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(4)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=1,len
                 c0     = 0.0_sp
                 t0     = real(i,kind=sp)
                 omt0   = om0*t0 
                 somt0  = sin(omt0)
                 do j=1,n
                    bjn   = bjb(j)
                    onep  = -1.0_sp**n
                    soma0 = sin(om0+n*Omega0)*t0
                    soms0 = onep*sin(om0-n*Omega0)*t0
                    c0    = c0 + bjn*(soma0+soms0)
                 end do
                 rhot(i)    = rho0*(1.0_sp+bj0b*somt0+c0)
                       
             end do
       end subroutine transmittance_spectr_r4


       subroutine transmittance_spectr_r8(rhot,len,bjb,n,rho0,Beta,om0,Omega0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmittance_spectr_r8
           !dir$ attributes forceinline ::   transmittance_spectr_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  transmittance_spectr_r8
           real(kind=dp), dimension(1:len), intent(out) :: rhot
           integer(kind=i4),                intent(in)  :: len
           real(kind=dp), dimension(1:n),   intent(in)  :: bjb
           integer(kind=i4),                intent(in)  :: n
           real(kind=dp),                   intent(in)  :: rho0
           real(kind=dp),                   intent(in)  :: Beta
           real(kind=dp),                   intent(in)  :: om0
           real(kind=dp),                   intent(in)  :: Omega0
           
           real(kind=dp), automatic :: omt0
           real(kind=dp), automatic :: t0
           real(kind=dp), automatic :: c0
           real(kind=dp), automatic :: somt0
           real(kind=dp), automatic :: soma0
           real(kind=dp), automatic :: soms0
           real(kind=dp), automatic :: bj0b,bjn,onep
           integer(kind=i4) :: j,i
          
           bj0b = bessel_jn(0,Beta)
          
            !dir$ assume_aligned rhot:64
            !dir$ assume_aligned bjb:64
            !dir$ vector aligned
            !dir$ ivdep
            !dir$ vector vectorlength(8)
            !dir$ vector multiple_gather_scatter_by_shuffles 
            !dir$ vector always
            do i=1,len
                 c0     = 0.0_dp
                 t0     = real(i,kind=dp)
                 omt0   = om0*t0 
                 somt0  = sin(omt0)
                 do j=1,n
                    bjn   = bjb(j)
                    onep  = -1.0_dp**n
                    soma0 = sin(om0+n*Omega0)*t0
                    soms0 = onep*sin(om0-n*Omega0)*t0
                    c0    = c0 + bjn*(soma0+soms0)
                 end do
                 rhot(i)    = rho0*(1.0_dp+bj0b*somt0+c0)
                       
             end do
       end subroutine transmittance_spectr_r8


       subroutine transmittance_spectr_exec_r4(rhot,len,bjb,n,rho0,Beta,om0,Omega0,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmittance_spectr_exec_r4
           real(kind=sp), dimension(1:len), intent(out) :: rhot
           integer(kind=i4),                intent(in)  :: len
           real(kind=sp), dimension(1:n),   intent(in)  :: bjb
           integer(kind=i4),                intent(in)  :: n
           real(kind=sp),                   intent(in)  :: rho0
           real(kind=sp),                   intent(in)  :: Beta
           real(kind=sp),                   intent(in)  :: om0
           real(kind=sp),                   intent(in)  :: Omega0
           integer(kind=i4),                intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                 call transmittance_spectr_unroll_16x_r4(rhot,len,bjb,n,rho0,Beta,om0,Omega0)
              case (8)
                 call transmittance_spectr_unroll_8x_r4(rhot,len,bjb,n,rho0,Beta,om0,Omega0)
              case (4)
                 call transmittance_spectr_unroll_4x_r4(rhot,len,bjb,n,rho0,Beta,om0,Omega0)
              case (2)
                 call transmittance_spectr_unroll_2x_r4(rhot,len,bjb,n,rho0,Beta,om0,Omega0)
              case (0)
                 call transmittance_spectr_r4(rhot,len,bjb,n,rho0,Beta,om0,Omega0)
              case default
                 return
              end select
       end subroutine transmittance_spectr_exec_r4


       subroutine transmittance_spectr_exec_r8(rhot,len,bjb,n,rho0,Beta,om0,Omega0,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  transmittance_spectr_exec_r8
           real(kind=dp), dimension(1:len), intent(out) :: rhot
           integer(kind=i4),                intent(in)  :: len
           real(kind=dp), dimension(1:n),   intent(in)  :: bjb
           integer(kind=i4),                intent(in)  :: n
           real(kind=dp),                   intent(in)  :: rho0
           real(kind=dp),                   intent(in)  :: Beta
           real(kind=dp),                   intent(in)  :: om0
           real(kind=dp),                   intent(in)  :: Omega0
           integer(kind=i4),                intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                 call transmittance_spectr_unroll_16x_r4(rhot,len,bjb,n,rho0,Beta,om0,Omega0)
              case (8)
                 call transmittance_spectr_unroll_8x_r4(rhot,len,bjb,n,rho0,Beta,om0,Omega0)
              case (4)
                 call transmittance_spectr_unroll_4x_r4(rhot,len,bjb,n,rho0,Beta,om0,Omega0)
              case (2)
                 call transmittance_spectr_unroll_2x_r4(rhot,len,bjb,n,rho0,Beta,om0,Omega0)
              case (0)
                 call transmittance_spectr_r4(rhot,len,bjb,n,rho0,Beta,om0,Omega0)
              case default
                 return
              end select
       end subroutine transmittance_spectr_exec_r8





       !МОДУЛЯЦИЯ ИЗЛУЧЕНИЯ
       !ВРАЩАЮЩИМСЯ СЕКТОРНЫМ РАСТРОМ
       !Formula 5, p. 205
       subroutine raster_transmitt_fft_r_c(rhophi_in,rhophi_out,dim_len,data_len,status)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: raster_transmitt_fft_r_c 
           use mkl_dfti
           use mkl_fft_wrappers, only : create_desc_r_c_1D,  &
                                        exec_fft_r_c_1D
           real(kind=dp),    dimension(data_len),  intent(in)  :: rhophi_in
           complex(kind=dp), dimension(data_len),  intent(out) :: rhophi_out
           integer(kind=i4),                       intent(in)  :: dim_len
           integer(kind=i4),                       intent(in)  :: data_len
           integer(kind=i4),                       intent(out) :: status
           type(DFTI_DESCRIPTOR), pointer :: handle
           logical(kind=i4), automatic :: callstack
           callstack = .true.
           call create_desc_r_c_1D(handle,dim_len,data_len,callstack,status)
           if(0 == status) then
              call exec_fft_r_c_1D(handle,rhophi_in,rhophi_out,data_len,1,callstack,status)
           end if
       end subroutine raster_transmitt_fft_r_c


       subroutine fourier_coeff_ak_r4(rhophi,cosphi,N,k,ak,ier)
           !dir$ optimize:3
           !dir$ attributes forceinline :: fourier_coeff_ak_r4 
           !dir$ attributes code_align : 32 :: fourier_coeff_ak_r4
           use quadpack, only : savint
           real(kind=sp),  dimension(1:128),      intent(in)    :: rhophi
           real(kind=sp),  dimension(1:128),      intent(inout) :: cosphi
           integer(kind=i4),                     intent(in)    :: N
           real(kind=sp),                        intent(in)    :: k
           real(kind=sp),                        intent(out)   :: ak
           integer(kind=i4),                     intent(inout) :: ier
           real(kind=sp), dimension(1:128) :: rpc,absc
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp), parameter :: step  = 128.0_sp
           real(kind=sp), automatic :: phit,hphit,dphi,carg,ratio,twopik,incr,fac,ndphi
           integer(kind=i4) :: i
           phit = twopi/real(N,kind=sp) 
           hphit= phit*0.5_sp
           dphi = hphit/step
           ndphi= -dphi
           twopik = twopi*k
           fac  = 2.0_sp/phit
           incr = 0.0_sp
           do i=1, 64
              incr      = incr - ndphi
              absc(i)   = incr
              ratio     = incr/phit
              cosphi(i) = cos(twopik*ratio)
              rpc(i)    = rhophi(i)*cosphi(i)
           end do
           incr = 0.0_sp
           do i=65, 128
              incr      = incr + dphi
              absc(i)   = incr
              ratio     = incr/phit
              cosphi(i) = cos(twopik*ratio)
              rpc(i)    = rhophi(i)*cosphi(i)
           end do
           call savint(rpc,absc,128,ndphi,dphi,ak,ier)
           ak = ak*phit
       end subroutine fourier_coeff_ak_r4


       subroutine fourier_coeff_a0k_r4(rhod,cosphi,N,k,a0k,ier)
           !dir$ optimize:3
           !dir$ attributes forceinline :: fourier_coeff_a0k_r4 
           !dir$ attributes code_align : 32 :: fourier_coeff_a0k_r4
           use quadpack, only : savint
           real(kind=sp), dimension(1:128),      intent(in)    :: rhod
           real(kind=sp), dimension(1:128),      intent(inout) :: cosphi
           integer(kind=i4),                     intent(in)    :: N
           real(kind=sp),                        intent(in)    :: k
           real(kind=sp),                        intent(out)   :: a0k
           integer(kind=i4),                     intent(inout) :: ier
           real(kind=sp), dimension(1:128) :: rpc,absc
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp), parameter :: pi2   = 1.57079632679489661923132169164_sp
           real(kind=sp), parameter :: step  = 64.0_sp
           real(kind=sp), automatic :: phit,ratio,twopik,dphi,incr,ndphi
           integer(kind=i4) :: i
           phit   = twopi/real(N,kind=sp)
           dphi   = pi2/step
           ndphi  = -pi2/step
           twopik = twopi*k
           incr   = 0.0_sp
           ratio  = twopik/phit
           do i=1, 64
              incr      = incr-ndphi
              absc(i)   = incr
              cosphi(i) = cos(ratio*incr)
              rpc(i)    = rhod(i)*cosphi(i) 
           end do
           incr = 0.0_sp
           do i = 65, 128
              incr = incr+dphi
              absc(i)  = incr
              cosphi(i) = cos(ratio*incr)
              rpc(i)    = rhod(i)*cosphi(i)
           end do
           call savint(rpc,absc,128,ndphi,dphi,a0k,ier)
       end subroutine fourier_coeff_a0k_r4


       subroutine fourier_coeff_ak_r8(rhophi,cosphi,N,k,ak,ier)
           !dir$ optimize:3
           !dir$ attributes forceinline :: fourier_coeff_ak_r8 
           !dir$ attributes code_align : 32 :: fourier_coeff_ak_r8
           use quadpack, only : davint
           real(kind=dp),  dimension(1:128),      intent(in)    :: rhophi
           real(kind=dp),  dimension(1:128),      intent(inout) :: cosphi
           integer(kind=i4),                     intent(in)    :: N
           real(kind=dp),                        intent(in)    :: k
           real(kind=dp),                        intent(out)   :: ak
           integer(kind=i4),                     intent(inout) :: ier
           real(kind=dp), dimension(1:128) :: rpc,absc
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp), parameter :: step  = 128.0_dp
           real(kind=dp), automatic :: phit,hphit,dphi,carg,ratio,twopik,incr,fac,ndphi
           integer(kind=i4) :: i,ier
           phit = twopi/real(N,kind=sp) 
           hphit= phit*0.5_dp
           dphi = hphit/step
           ndphi= -dphi
           twopik = twopi*k
           fac  = 2.0_dp/phit
           incr = 0.0_dp
           do i=1, 64
              incr      = incr - ndphi
              absc(i)   = incr
              ratio     = incr/phit
              cosphi(i) = cos(twopik*ratio)
              rpc(i)    = rhophi(i)*cosphi(i)
           end do
           incr = 0.0_dp
           do i=65, 128
              incr      = incr + dphi
              absc(i)   = incr
              ratio     = incr/phit
              cosphi(i) = cos(twopik*ratio)
              rpc(i)    = rhophi(i)*cosphi(i)
           end do
           call davint(rpc,absc,128,ndphi,dphi,ak,ier)
           ak = ak*phit
       end subroutine fourier_coeff_ak_r8


       subroutine fourier_coeff_a0k_r8(rhod,cosphi,N,k,a0k,ier)
           !dir$ optimize:3
           !dir$ attributes forceinline :: fourier_coeff_a0k_r8
           !dir$ attributes code_align : 32 :: fourier_coeff_a0k_r8
           use quadpack, only : davint
           real(kind=dp), dimension(1:128),      intent(in)    :: rhod
           real(kind=dp), dimension(1:128),      intent(inout) :: cosphi
           integer(kind=i4),                     intent(in)    :: N
           real(kind=dp),                        intent(in)    :: k
           real(kind=dp),                        intent(out)   :: a0k
           integer(kind=i4),                     intent(inout) :: ier
           real(kind=dp), dimension(1:128) :: rpc,absc
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp), parameter :: pi2   = 1.57079632679489661923132169164_dp
           real(kind=dp), parameter :: step  = 64.0_dp
           real(kind=dp), automatic :: phit,ratio,twopik,dphi,incr,ndphi
           integer(kind=i4) :: i
           phit   = twopi/real(N,kind=dp)
           dphi   = pi2/step
           ndphi  = -pi2/step
           twopik = twopi*k
           incr   = 0.0_dp
           ratio  = twopik/phit
           do i=1, 64
              incr      = incr-ndphi
              absc(i)   = incr
              cosphi(i) = cos(ratio*incr)
              rpc(i)    = rhod(i)*cosphi(i) 
           end do
           incr = 0.0_dp
           do i = 65, 128
              incr = incr+dphi
              absc(i)  = incr
              cosphi(i) = cos(ratio*incr)
              rpc(i)    = rhod(i)*cosphi(i)
           end do
           call davint(rpc,absc,128,ndphi,dphi,a0k,ier)
       end subroutine fourier_coeff_a0k_r8


       subroutine fourier_coeff_bk_r4(rhophi,cosphi,N,k,ak,ier)
           !dir$ optimize:3
           !dir$ attributes forceinline :: fourier_coeff_bk_r4 
           !dir$ attributes code_align : 32 :: fourier_coeff_bk_r4
           use quadpack, only : savint
           real(kind=sp),  dimension(1:128),      intent(in)    :: rhophi
           real(kind=sp),  dimension(1:128),      intent(inout) :: cosphi
           integer(kind=i4),                     intent(in)    :: N
           real(kind=sp),                        intent(in)    :: k
           real(kind=sp),                        intent(out)   :: ak
           integer(kind=i4),                     intent(inout) :: ier
           real(kind=sp), dimension(1:128) :: rpc,absc
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp), parameter :: step  = 128.0_sp
           real(kind=sp), automatic :: phit,hphit,dphi,carg,ratio,twopik,incr,fac,ndphi
           integer(kind=i4) :: i,ier
           phit = twopi/real(N,kind=sp) 
           hphit= phit*0.5_sp
           dphi = hphit/step
           ndphi= -dphi
           twopik = twopi*k
           fac  = 2.0_sp/phit
           incr = 0.0_sp
           do i=1, 64
              incr      = incr - ndphi
              absc(i)   = incr
              ratio     = incr/phit
              cosphi(i) = sin(twopik*ratio)
              rpc(i)    = rhophi(i)*cosphi(i)
           end do
           incr = 0.0_sp
           do i=65, 128
              incr      = incr + dphi
              absc(i)   = incr
              ratio     = incr/phit
              cosphi(i) = sin(twopik*ratio)
              rpc(i)    = rhophi(i)*cosphi(i)
           end do
           call savint(rpc,absc,128,ndphi,dphi,ak,ier)
           ak = ak*phit
       end subroutine fourier_coeff_bk_r4


       subroutine fourier_coeff_b0k_r4(rhod,cosphi,N,k,a0k,ier)
           !dir$ optimize:3
           !dir$ attributes forceinline :: fourier_coeff_b0k_r4 
           !dir$ attributes code_align : 32 :: fourier_coeff_b0k_r4
           use quadpack, only : savint
           real(kind=sp), dimension(1:128),      intent(in)    :: rhod
           real(kind=sp), dimension(1:128),      intent(inout) :: cosphi
           integer(kind=i4),                     intent(in)    :: N
           real(kind=sp),                        intent(in)    :: k
           real(kind=sp),                        intent(out)   :: a0k
           integer(kind=i4),                     intent(inout) :: ier
           real(kind=sp), dimension(1:128) :: rpc,absc
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp), parameter :: pi2   = 1.57079632679489661923132169164_sp
           real(kind=sp), parameter :: step  = 64.0_sp
           real(kind=sp), automatic :: phit,ratio,twopik,dphi,incr,ndphi
           integer(kind=i4) :: i
           phit   = twopi/real(N,kind=sp)
           dphi   = pi2/step
           ndphi  = -pi2/step
           twopik = twopi*k
           incr   = 0.0_sp
           ratio  = twopik/phit
           do i=1, 64
              incr      = incr-ndphi
              absc(i)   = incr
              cosphi(i) = sin(ratio*incr)
              rpc(i)    = rhod(i)*cosphi(i) 
           end do
           incr = 0.0_sp
           do i = 65, 128
              incr = incr+dphi
              absc(i)  = incr
              cosphi(i) = sin(ratio*incr)
              rpc(i)    = rhod(i)*cosphi(i)
           end do
           call savint(rpc,absc,128,ndphi,dphi,a0k,ier)
       end subroutine fourier_coeff_b0k_r4


       subroutine fourier_coeff_bk_r8(rhophi,cosphi,absc,N,k,ak,ier)
           !dir$ optimize:3
           !dir$ attributes forceinline :: fourier_coeff_bk_r8 
           !dir$ attributes code_align : 32 :: fourier_coeff_bk_r8
           use quadpack, only : davint
           real(kind=dp),  dimension(1:128),      intent(in)    :: rhophi
           real(kind=dp),  dimension(1:128),      intent(inout) :: cosphi
           real(kind=sp),  dimension(1:128),      intent(inout) :: absc
           integer(kind=i4),                     intent(in)    :: N
           real(kind=dp),                        intent(in)    :: k
           real(kind=dp),                        intent(out)   :: ak
           integer(kind=i4),                     intent(inout) :: ier
                     
           real(kind=dp), dimension(1:128) :: rpc
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp), parameter :: step  = 128.0_dp
           real(kind=dp), automatic :: phit,hphit,dphi,carg,ratio,twopik,incr,fac,ndphi
           integer(kind=i4) :: i,ier
           phit = twopi/real(N,kind=sp) 
           hphit= phit*0.5_dp
           dphi = hphit/step
           ndphi= -dphi
           twopik = twopi*k
           fac  = 2.0_dp/phit
           incr = 0.0_dp
           do i=1, 64
              incr      = incr - ndphi
              absc(i)   = incr
              ratio     = incr/phit
              cosphi(i) = sin(twopik*ratio)
              rpc(i)    = rhophi(i)*cosphi(i)
           end do
           incr = 0.0_dp
           do i=65, 128
              incr      = incr + dphi
              absc(i)   = incr
              ratio     = incr/phit
              cosphi(i) = sin(twopik*ratio)
              rpc(i)    = rhophi(i)*cosphi(i)
           end do
           call davint(rpc,absc,128,ndphi,dphi,ak,ier)
           ak = ak*phit
       end subroutine fourier_coeff_bk_r8


       subroutine fourier_coeff_b0k_r8(rhod,cosphi,N,k,a0k,ier)
           !dir$ optimize:3
           !dir$ attributes forceinline :: fourier_coeff_b0k_r8
           !dir$ attributes code_align : 32 :: fourier_coeff_b0k_r8
           use quadpack, only : davint
           real(kind=dp), dimension(1:128),      intent(in)    :: rhod
           real(kind=dp), dimension(1:128),      intent(inout) :: cosphi
           integer(kind=i4),                     intent(in)    :: N
           real(kind=dp),                        intent(in)    :: k
           real(kind=dp),                        intent(out)   :: a0k
           integer(kind=i4),                     intent(inout) :: ier
           real(kind=dp), dimension(1:128) :: rpc,absc
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp), parameter :: pi2   = 1.57079632679489661923132169164_dp
           real(kind=dp), parameter :: step  = 64.0_dp
           real(kind=dp), automatic :: phit,ratio,twopik,dphi,incr,ndphi
           integer(kind=i4) :: i
           phit   = twopi/real(N,kind=dp)
           dphi   = pi2/step
           ndphi  = -pi2/step
           twopik = twopi*k
           incr   = 0.0_dp
           ratio  = twopik/phit
           do i=1, 64
              incr      = incr-ndphi
              absc(i)   = incr
              cosphi(i) = sin(ratio*incr)
              rpc(i)    = rhod(i)*cosphi(i) 
           end do
           incr = 0.0_dp
           do i = 65, 128
              incr = incr+dphi
              absc(i)  = incr
              cosphi(i) = sin(ratio*incr)
              rpc(i)    = rhod(i)*cosphi(i)
           end do
           call davint(rpc,absc,128,ndphi,dphi,a0k,ier)
       end subroutine fourier_coeff_b0k_r8


       subroutine rhod_integral_r4(rhod,n,phi1,phi2,


       subroutine fourier_coeff_a0_r4(rhophi,absc,phit,xlo,xup,ans,ier)
           !dir$ optimize:3
           !dir$ attributes forceinline :: fourier_coeff_a0_r4
           !dir$ attributes code_align : 32 :: fourier_coeff_a0_r4
           use quadpack, only : savint
           real(kind=sp),  dimension(1:128),  intent(in) :: rhophi
           real(kind=sp),  dimension(1:128),  intent(in) :: absc
           real(kind=sp),                    intent(in) :: phit
           real(kind=sp),                    intent(in) :: xlo
           real(kind=sp),                    intent(in) :: xup
           real(kind=sp),                    intent(out):: ans
           integer(kind=i4),                 intent(out):: ier
           real(kind=sp) :: twophit
           twophit = 2.0_sp/phit
           call savint(rhophi,absc,128,xlo,xup,ans,ier)
           ans = twophit*ans
       end subroutine fourier_coeff_a0_r4
       

       subroutine fourier_coeff_a0_r8(rhophi,absc,phit,xlo,xup,ans,ier)
           !dir$ optimize:3
           !dir$ attributes forceinline :: fourier_coeff_a0_r8
           !dir$ attributes code_align : 32 :: fourier_coeff_a0_r8
           use quadpack, only : davint
           real(kind=dp),  dimension(1:128),  intent(in) :: rhophi
           real(kind=dp),  dimension(1:128),  intent(in) :: absc
           real(kind=dp),                     intent(in) :: phit
           real(kind=dp),                     intent(in) :: xlo
           real(kind=dp),                     intent(in) :: xup
           real(kind=dp),                     intent(out):: ans
           integer(kind=i4),                  intent(out):: ier
           real(kind=dp) :: twophit
           twophit = 2.0_dp/phit
           call davint(rhophi,absc,128,xlo,xup,ans,ier)
           ans = twophit*ans
       end subroutine fourier_coeff_a0_r8


       subroutine raster_transmitt_fourier_phi_t_r4(rhophit,len,rhophi,cosphi, &
                                                absc,N,ans,r,klim,xlo,xup)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: raster_transmitt_fourier_phi_t_r4
           real(kind=sp),  dimension(1:len),     intent(out)   :: rhophit
           integer(kind=i4),                     intent(in)    :: len
           real(kind=sp),  dimension(1:128),     intent(in)    :: rhophi
           real(kind=sp),  dimension(1:128),     intent(inout) :: cosphi
           real(kind=sp),  dimension(1:128),     intent(in)    :: absc
           integer(kind=i4),                     intent(in)    :: N
           real(kind=sp),                        intent(in)    :: ans !a0
           integer(kind=i4),                     intent(in)    :: r
           integer(kind=i4),                     intent(in)    :: klim
           real(kind=sp),                        intent(in)    :: xlo
           real(kind=sp),                        intent(in)    :: xup
           real(kind=dp), parameter :: step  = 128.0_sp
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp), automatic :: t0,k0,aks,akc,bk,phit,phi,Omeg0,cterm,sterm,&
                                       trigsum,trigdiff,ratio,phinc,arg1,arg2,sum, &
                                       iers,ierc,ctom,stom,omt,arg3,c0,s0,a0,ier,x0
           integer(kind=i4) :: i,k,ier
           phit  = twopi/real(N,kind=sp)
           Omega0= twopi*real(r,kind=sp)
           phinc = twopi/step
           call fourier_coeff_a0_r4(rhophi,absc,phit,xlo,xup,a0,ier)
           a0 = 0.5_sp*a0
           do i=1, len
              t0   = real(i,kind=sp)
              phi  = phi+phinc
              omt  = Omega0*t0
              sum  = 0.0_sp
              akc  = 0.0_sp
              aks  = 0.0_sp
              ierc = 0
              iers = 0
              do k=1, klim
                 k0      = real(k,kind=sp)
                 arg1    = (twopi*k0)/phit
                 arg2    = arg1*phi
                 cterm   = cos(arg2)
                 sterm   = sin(arg2)
                 call fourier_coeff_ak_r4(rhophi,cosphi,N,k0,akc,ierc)
                 call fourier_coeff_bk_r4(rhophi,cosphi,N,k0,aks,iers)
                 arg3    = arg1*omt
                 stom    = sin(arg3)
                 ctom    = cos(arg3)
                 trigsum = ak*cterm+bk*sterm
                 trigdiff= ak*sterm-bk*cterm
                 c0      = trigsum*ctom
                 s0      = trigdiff*stom
                 x0      = c0+s0
                 sum     = sum+x0
               end do
               rhophit(i) = a0*sum
           end do
       end subroutine raster_transmitt_fourier_phi_t_r4

       !
       !Formula 2, p. 208
       subroutine raster_transmitt_fourier_t_r4(rhot,len,rhophi,rhod,cosphi1,cosphi2,absc, &
                                                absc2,N,xlo,xup,r,klim,phi1,phi2)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: raster_transmitt_fourier_t_r4
           use quadpack, only : savint
           real(kind=sp), dimension(1:len),      intent(out)   :: rhot
           integer(kind=i4),                     intent(in)    :: len
           real(kind=sp),  dimension(1:128),     intent(in)    :: rhophi
           real(kind=sp),  dimension(1:128),     intent(in)    :: rhod
           real(kind=sp),  dimension(1:128),     intent(inout) :: cosphi1
           real(kind=sp),  dimension(1:128),     intent(inout) :: cosphi2
           real(kind=sp),  dimension(1:128),     intent(in)    :: absc
           real(kind=sp),  dimension(1:128),     intent(in)    :: absc2 ! abscissas for rhod integration
           integer(kind=i4),                     intent(in)    :: N
           real(kind=sp),                        intent(in)    :: xlo
           real(kind=sp),                        intent(in)    :: xup
           integer(kind=i4),                     intent(in)    :: r   ! number of raster rotations
           integer(kind=i4),                     intent(in)    :: klim
           real(kind=sp),                        intent(in)    :: phi1
           real(kind=sp),                        intent(in)    :: phi2
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp), automatic :: k0,t0,aks,akc,phit,Omega0,x1,x2, &
                                       ratio,arg1, cterm,sterm,         &
                                       sum,ak0s,ak0c,ier0s,ier0c,iers,ierc,   &
                                       c0,x0,omt,stom,ctom,a0,rd,fac
           integer(kind=i4) :: i,k,ier
           phit  = twopi/real(N,kind=sp)
           Omega0= twopi*real(r,kind=sp)
           call fourier_coeff_a0_r4(rhophi,absc,phit,xlo,xup,a0,ier)
           a0 = 0.5_sp*a0
           ier = 0
           call savint(rhod,absc2,128,phi1,phi2,rd,ier)
           fac = a0*rd
           do i=1, len
              t0  = real(i,kind=sp)
              omt = Omega0*t0
              sum = 0.0_sp
              do k=1, klim
                 k0 = real(k,kind=sp)
                 call fourier_coeff_ak_r4(rhophi,cosphi,N,k0,akc,ierc)
                 call fourier_coeff_bk_r4(rhophi,cosphi,N,k0,aks,iers)
                 ratio = twopi*k0/phit
                 arg1  = ratio*omt
                 ctom  = cos(arg1)
                 stom  = sin(arg1)
                 call fourier_coeff_a0k_r4(rhod,cosphi,N,k0,ak0c,ier0c)
                 call fourier_coeff_b0k_r4(rhod,cosphi,N,k0,ak0s,ier0s)
                 x1    = akc*ak0c+aks*ak0s
                 x2    = akc*ak0c-aks*ak0s  
                 cterm = x1*ctom
                 sterm = x2*stom
                 c0    = cterm+sterm
                 sum   = sum+c0
              end do
              rhot(i) = fac*sum
           end do
       end subroutine raster_transmitt_fourier_t_r4


       subroutine raster_transmitt_fourier_t_r4_omp(rhot,len,rhophi,rhod,cosphi1,cosphi2,absc, &
                                                    absc2,N,xlo,xup,r,klim,phi1,phi2)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: raster_transmitt_fourier_t_r4_omp
           use quadpack, only : savint
           real(kind=sp), dimension(1:len),      intent(out)   :: rhot
           integer(kind=i4),                     intent(in)    :: len
           real(kind=sp),  dimension(1:128),     intent(in)    :: rhophi
           real(kind=sp),  dimension(1:128),     intent(in)    :: rhod
           real(kind=sp),  dimension(1:128),     intent(inout) :: cosphi1
           real(kind=sp),  dimension(1:128),     intent(inout) :: cosphi2
           real(kind=sp),  dimension(1:128),     intent(in)    :: absc
           real(kind=sp),  dimension(1:128),     intent(in)    :: absc2 ! abscissas for rhod integration
           integer(kind=i4),                     intent(in)    :: N
           real(kind=sp),                        intent(in)    :: xlo
           real(kind=sp),                        intent(in)    :: xup
           integer(kind=i4),                     intent(in)    :: r   ! number of raster rotations
           integer(kind=i4),                     intent(in)    :: klim
           real(kind=sp),                        intent(in)    :: phi1
           real(kind=sp),                        intent(in)    :: phi2
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp), automatic :: k0,t0,aks,akc,phit,Omega0,x1,x2, &
                                       ratio,arg1, cterm,sterm,         &
                                       sum,ak0s,ak0c,ier0s,ier0c,iers,ierc,   &
                                       c0,x0,omt,stom,ctom,a0,rd,fac
           integer(kind=i4) :: i,k,ier
           phit  = twopi/real(N,kind=sp)
           Omega0= twopi*real(r,kind=sp)
           call fourier_coeff_a0_r4(rhophi,absc,phit,xlo,xup,a0,ier)
           a0 = 0.5_sp*a0
           ier = 0
           call savint(rhod,absc2,128,phi1,phi2,rd,ier)
           fac = a0*rd
!$omp parallel do default(none) schedule(runtime)     &
!$omp private(i,t0,omt,sum,k,k0,ratio,arg1,ctom,stom) &
!$omp private(x1,x2,cterm,sterm,c0,akc,aks,ak0c,ak0s) &
!$omp private(ierc,iers,ier0c,ier0s,rhophi,cosphi,rhod)
!$omp shared(rhot,len,klim,Omega0,N,twopi,phit,ratio,fac)
           do i=1, len
              t0  = real(i,kind=sp)
              omt = Omega0*t0
              sum = 0.0_sp
              do k=1, klim
                 k0 = real(k,kind=sp)
                 call fourier_coeff_ak_r4(rhophi,cosphi,N,k0,akc,ierc)
                 call fourier_coeff_bk_r4(rhophi,cosphi,N,k0,aks,iers)
                 ratio = twopi*k0/phit
                 arg1  = ratio*omt
                 ctom  = cos(arg1)
                 stom  = sin(arg1)
                 call fourier_coeff_a0k_r4(rhod,cosphi,N,k0,ak0c,ier0c)
                 call fourier_coeff_b0k_r4(rhod,cosphi,N,k0,ak0s,ier0s)
                 x1    = akc*ak0c+aks*ak0s
                 x2    = akc*ak0c-aks*ak0s  
                 cterm = x1*ctom
                 sterm = x2*stom
                 c0    = cterm+sterm
                 sum   = sum+c0
              end do
              rhot(i) = fac*sum
           end do
!$omp end parallel do
       end subroutine raster_transmitt_fourier_t_r4_omp
                                                

                                                

       subroutine raster_transmitt_fourier_phi_t_r8(rhophit,len,rhophi,cosphi, &
                                                absc,N,ans,r,klim,xlo,xup)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: raster_transmitt_fourier_phi_t_r8
           real(kind=dp),  dimension(1:len),     intent(out)   :: rhophit
           integer(kind=i4),                     intent(in)    :: len
           real(kind=dp),  dimension(1:128),     intent(in)    :: rhophi
           real(kind=dp),  dimension(1:128),     intent(inout) :: cosphi
           real(kind=dp),  dimension(1:128),     intent(in)    :: absc
           integer(kind=i4),                     intent(in)    :: N
           real(kind=dp),                        intent(in)    :: ans !a0
           integer(kind=i4),                     intent(in)    :: r
           integer(kind=i4),                     intent(in)    :: klim
           real(kind=dp),                        intent(in)    :: xlo
           real(kind=dp),                        intent(in)    :: xup
           real(kind=dp), parameter :: step  = 128.0_dp
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp), automatic :: t0,k0,aks,akc,bk,phit,phi,Omeg0,cterm,sterm,&
                                       trigsum,trigdiff,ratio,phinc,arg1,arg2,sum, &
                                       iers,ierc,ctom,stom,omt,arg3,c0,s0,a0,ier,x0
           integer(kind=i4) :: i,k,ier
           phit  = twopi/real(N,kind=dp)
           Omega0= twopi*real(r,kind=dp)
           phinc = twopi/step
           call fourier_coeff_a0_r8(rhophi,absc,phit,xlo,xup,a0,ier)
           a0 = 0.5_dp*a0
           do i=1, len
              t0   = real(i,kind=dp)
              phi  = phi+phinc
              omt  = Omega0*t0
              sum  = 0.0_dp
              akc  = 0.0_dp
              aks  = 0.0_dp
              ierc = 0
              iers = 0
              do k=1, klim
                 k0      = real(k,kind=dp)
                 arg1    = (twopi*k0)/phit
                 arg2    = arg1*phi
                 cterm   = cos(arg2)
                 sterm   = sin(arg2)
                 call fourier_coeff_ak_r8(rhophi,cosphi,N,k0,akc,ierc)
                 call fourier_coeff_bk_r8(rhophi,cosphi,N,k0,aks,iers)
                 arg3    = arg1*omt
                 stom    = sin(arg3)
                 ctom    = cos(arg3)
                 trigsum = ak*cterm+bk*sterm
                 trigdiff= ak*sterm-bk*cterm
                 c0      = trigsum*ctom
                 s0      = trigdiff*stom
                 x0      = c0+s0
                 sum     = sum+x0
               end do
               rhophit(i) = a0*sum
           end do
       end subroutine raster_transmitt_fourier_phi_t_r8


       


       subroutine raster_transmitt_fourier_t_r8(rhot,len,rhophi,rhod,cosphi1,cosphi2,absc, &
                                                absc2,N,xlo,xup,r,klim,phi1,phi2)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: raster_transmitt_fourier_t_r8
           use quadpack, only : davint
           real(kind=dp), dimension(1:len),      intent(out)   :: rhot
           integer(kind=i4),                     intent(in)    :: len
           real(kind=dp),  dimension(1:128),     intent(in)    :: rhophi
           real(kind=dp),  dimension(1:128),     intent(in)    :: rhod
           real(kind=dp),  dimension(1:128),     intent(inout) :: cosphi1
           real(kind=dp),  dimension(1:128),     intent(inout) :: cosphi2
           real(kind=dp),  dimension(1:128),     intent(in)    :: absc
           real(kind=dp),  dimension(1:128),     intent(in)    :: absc2 ! abscissas for rhod integration
           integer(kind=i4),                     intent(in)    :: N
           real(kind=dp),                        intent(in)    :: xlo
           real(kind=dp),                        intent(in)    :: xup
           integer(kind=i4),                     intent(in)    :: r   ! number of raster rotations
           integer(kind=i4),                     intent(in)    :: klim
           real(kind=dp),                        intent(in)    :: phi1
           real(kind=dp),                        intent(in)    :: phi2
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp), automatic :: k0,t0,aks,akc,phit,Omega0,x1,x2, &
                                       ratio,arg1, cterm,sterm,         &
                                       sum,ak0s,ak0c,ier0s,ier0c,iers,ierc,   &
                                       c0,x0,omt,stom,ctom,a0,rd,fac
           integer(kind=i4) :: i,k,ier
           phit  = twopi/real(N,kind=dp)
           Omega0= twopi*real(r,kind=dp)
           call fourier_coeff_a0_r8(rhophi,absc,phit,xlo,xup,a0,ier)
           a0 = 0.5_dp*a0
           ier = 0
           call davint(rhod,absc2,128,phi1,phi2,rd,ier)
           fac = a0*rd
           do i=1, len
              t0  = real(i,kind=dp)
              omt = Omega0*t0
              sum = 0.0_dp
              do k=1, klim
                 k0 = real(k,kind=dp)
                 call fourier_coeff_ak_r8(rhophi,cosphi,N,k0,akc,ierc)
                 call fourier_coeff_bk_r8(rhophi,cosphi,N,k0,aks,iers)
                 ratio = twopi*k0/phit
                 arg1  = ratio*omt
                 ctom  = cos(arg1)
                 stom  = sin(arg1)
                 call fourier_coeff_a0k_r8(rhod,cosphi,N,k0,ak0c,ier0c)
                 call fourier_coeff_b0k_r8(rhod,cosphi,N,k0,ak0s,ier0s)
                 x1    = akc*ak0c+aks*ak0s
                 x2    = akc*ak0c-aks*ak0s  
                 cterm = x1*ctom
                 sterm = x2*stom
                 c0    = cterm+sterm
                 sum   = sum+c0
              end do
              rhot(i) = fac*sum
           end do
       end subroutine raster_transmitt_fourier_t_r8


       subroutine raster_transmitt_fourier_t_r8_omp(rhot,len,rhophi,rhod,cosphi1,cosphi2,absc, &
                                                    absc2,N,xlo,xup,r,klim,phi1,phi2)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: raster_transmitt_fourier_t_r8_omp
           use quadpack, only : davint
           real(kind=dp), dimension(1:len),      intent(out)   :: rhot
           integer(kind=i4),                     intent(in)    :: len
           real(kind=dp),  dimension(1:128),     intent(in)    :: rhophi
           real(kind=dp),  dimension(1:128),     intent(in)    :: rhod
           real(kind=dp),  dimension(1:128),     intent(inout) :: cosphi1
           real(kind=dp),  dimension(1:128),     intent(inout) :: cosphi2
           real(kind=dp),  dimension(1:128),     intent(in)    :: absc
           real(kind=dp),  dimension(1:128),     intent(in)    :: absc2 ! abscissas for rhod integration
           integer(kind=i4),                     intent(in)    :: N
           real(kind=dp),                        intent(in)    :: xlo
           real(kind=dp),                        intent(in)    :: xup
           integer(kind=i4),                     intent(in)    :: r   ! number of raster rotations
           integer(kind=i4),                     intent(in)    :: klim
           real(kind=dp),                        intent(in)    :: phi1
           real(kind=dp),                        intent(in)    :: phi2
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp), automatic :: k0,t0,aks,akc,phit,Omega0,x1,x2, &
                                       ratio,arg1, cterm,sterm,         &
                                       sum,ak0s,ak0c,ier0s,ier0c,iers,ierc,   &
                                       c0,x0,omt,stom,ctom,a0,rd,fac
           integer(kind=i4) :: i,k,ier
           phit  = twopi/real(N,kind=dp)
           Omega0= twopi*real(r,kind=dp)
           call fourier_coeff_a0_r8(rhophi,absc,phit,xlo,xup,a0,ier)
           a0 = 0.5_dp*a0
           ier = 0
           call davint(rhod,absc2,128,phi1,phi2,rd,ier)
           fac = a0*rd
!$omp parallel do default(none) schedule(runtime)     &
!$omp private(i,t0,omt,sum,k,k0,ratio,arg1,ctom,stom) &
!$omp private(x1,x2,cterm,sterm,c0,akc,aks,ak0c,ak0s) &
!$omp private(ierc,iers,ier0c,ier0s,rhophi,cosphi,rhod)
!$omp shared(rhot,len,klim,Omega0,N,twopi,phit,ratio,fac)
           do i=1, len
              t0  = real(i,kind=dp)
              omt = Omega0*t0
              sum = 0.0_dp
              do k=1, klim
                 k0 = real(k,kind=dp)
                 call fourier_coeff_ak_r8(rhophi,cosphi,N,k0,akc,ierc)
                 call fourier_coeff_bk_r8(rhophi,cosphi,N,k0,aks,iers)
                 ratio = twopi*k0/phit
                 arg1  = ratio*omt
                 ctom  = cos(arg1)
                 stom  = sin(arg1)
                 call fourier_coeff_a0k_r8(rhod,cosphi,N,k0,ak0c,ier0c)
                 call fourier_coeff_b0k_r8(rhod,cosphi,N,k0,ak0s,ier0s)
                 x1    = akc*ak0c+aks*ak0s
                 x2    = akc*ak0c-aks*ak0s  
                 cterm = x1*ctom
                 sterm = x2*stom
                 c0    = cterm+sterm
                 sum   = sum+c0
              end do
              rhot(i) = fac*sum
           end do
!$omp end parallel do
       end subroutine raster_transmitt_fourier_t_r8
                                                

                                                


       subroutine raster_transmitt_fourier_phi_t_r4_omp(rhophit,len,rhophi,cosphi, &
                                                   absc,N,ans,r,klim,xlo,xup)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: raster_transmitt_fourier_phi_t_r4
           use omp_lib
           real(kind=sp),  dimension(1:len),     intent(out)   :: rhophit
           integer(kind=i4),                     intent(in)    :: len
           real(kind=sp),  dimension(1:128),     intent(in)    :: rhophi
           real(kind=sp),  dimension(1:128),     intent(inout) :: cosphi
           real(kind=sp),  dimension(1:128),     intent(in)    :: absc
           integer(kind=i4),                     intent(in)    :: N
           real(kind=sp),                        intent(in)    :: ans !a0
           integer(kind=i4),                     intent(in)    :: r
           integer(kind=i4),                     intent(in)    :: klim
           real(kind=sp),                        intent(in)    :: xlo
           real(kind=sp),                        intent(in)    :: xup
           real(kind=dp), parameter :: step  = 128.0_sp
           real(kind=sp), parameter :: twopi = 6.283185307179586476925286766559_sp
           real(kind=sp), automatic :: t0,k0,aks,akc,bk,phit,phi,Omeg0,cterm,sterm,&
                                       trigsum,trigdiff,ratio,phinc,arg1,arg2,sum, &
                                       iers,ierc,ctom,stom,omt,arg3,c0,s0,a0,ier,x0
           integer(kind=i4) :: i,k,ier
           phit  = twopi/real(N,kind=sp)
           Omega0= twopi*real(r,kind=sp)
           phinc = twopi/step
           call fourier_coeff_a0_r4(rhophi,absc,phit,xlo,xup,a0,ier)
           a0 = 0.5_sp*a0
!$omp parallel do schedule(runtime) default(none) &
!$omp private(i,t0,phi,omt,sum,akc,aks,ierc,iers) &
!$omp private(k,k0,arg1,arg2,arg3,cterm,sterm)    &
!$omp private(stom,trigsum,trigdiff,c0,s0,x0)     &
!$omp shared(rhophit,len,klim,phinc,Omega0)       &
!$omp shared(rhophi,cosphi,a0)
           do i=1, len
              t0   = real(i,kind=sp)
              phi  = phi+phinc
              omt  = Omega0*t0
              sum  = 0.0_sp
              akc  = 0.0_sp
              aks  = 0.0_sp
              ierc = 0
              iers = 0
              do k=1, klim
                 k0      = real(k,kind=sp)
                 arg1    = (twopi*k0)/phit
                 arg2    = arg1*phi
                 cterm   = cos(arg2)
                 sterm   = sin(arg2)
                 call fourier_coeff_ak_r4(rhophi,cosphi,N,k0,akc,ierc)
                 call fourier_coeff_bk_r4(rhophi,cosphi,N,k0,aks,iers)
                 arg3    = arg1*omt
                 stom    = sin(arg3)
                 ctom    = cos(arg3)
                 trigsum = ak*cterm+bk*sterm
                 trigdiff= ak*sterm-bk*cterm
                 c0      = trigsum*ctom
                 s0      = trigdiff*stom
                 x0      = c0+s0
                 sum     = sum+x0
               end do
               rhophit(i) = a0*sum
           end do
!$omp end parallel do
       end subroutine raster_transmitt_fourier_phi_t_r4


       subroutine raster_transmitt_fourier_phi_t_r8_omp(rhophit,len,rhophi,cosphi, &
                                                   absc,N,ans,r,klim,xlo,xup)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: raster_transmitt_fourier_phi_t_r8
           use omp_lib
           real(kind=dp),  dimension(1:len),     intent(out)   :: rhophit
           integer(kind=i4),                     intent(in)    :: len
           real(kind=dp),  dimension(1:128),     intent(in)    :: rhophi
           real(kind=dp),  dimension(1:128),     intent(inout) :: cosphi
           real(kind=dp),  dimension(1:128),     intent(in)    :: absc
           integer(kind=i4),                     intent(in)    :: N
           real(kind=dp),                        intent(in)    :: ans !a0
           integer(kind=i4),                     intent(in)    :: r
           integer(kind=i4),                     intent(in)    :: klim
           real(kind=dp),                        intent(in)    :: xlo
           real(kind=dp),                        intent(in)    :: xup
           real(kind=dp), parameter :: step  = 128.0_dp
           real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
           real(kind=dp), automatic :: t0,k0,aks,akc,bk,phit,phi,Omeg0,cterm,sterm,&
                                       trigsum,trigdiff,ratio,phinc,arg1,arg2,sum, &
                                       iers,ierc,ctom,stom,omt,arg3,c0,s0,a0,ier,x0
           integer(kind=i4) :: i,k,ier
           phit  = twopi/real(N,kind=dp)
           Omega0= twopi*real(r,kind=dp)
           phinc = twopi/step
           call fourier_coeff_a0_r8(rhophi,absc,phit,xlo,xup,a0,ier)
           a0 = 0.5_dp*a0
!$omp parallel do schedule(runtime) default(none) &
!$omp private(i,t0,phi,omt,sum,akc,aks,ierc,iers) &
!$omp private(k,k0,arg1,arg2,arg3,cterm,sterm)    &
!$omp private(stom,trigsum,trigdiff,c0,s0,x0)     &
!$omp shared(rhophit,len,klim,phinc,Omega0)       &
!$omp shared(rhophi,cosphi,a0)
           do i=1, len
              t0   = real(i,kind=dp)
              phi  = phi+phinc
              omt  = Omega0*t0
              sum  = 0.0_dp
              akc  = 0.0_dp
              aks  = 0.0_dp
              ierc = 0
              iers = 0
              do k=1, klim
                 k0      = real(k,kind=dp)
                 arg1    = (twopi*k0)/phit
                 arg2    = arg1*phi
                 cterm   = cos(arg2)
                 sterm   = sin(arg2)
                 call fourier_coeff_ak_r8(rhophi,cosphi,N,k0,akc,ierc)
                 call fourier_coeff_bk_r8(rhophi,cosphi,N,k0,aks,iers)
                 arg3    = arg1*omt
                 stom    = sin(arg3)
                 ctom    = cos(arg3)
                 trigsum = ak*cterm+bk*sterm
                 trigdiff= ak*sterm-bk*cterm
                 c0      = trigsum*ctom
                 s0      = trigdiff*stom
                 x0      = c0+s0
                 sum     = sum+x0
               end do
               rhophit(i) = a0*sum
           end do
!$omp end parallel do
       end subroutine raster_transmitt_fourier_phi_t_r8

       
       !Formula 1, p. 208                                         
       pure elemental function rho_diaphr_integral_r4(p2phi,p1phi,rhophi,sigma) result(rhod)
           !dir$ optimize:3
           !dir$ attributes forceinline :: rho_diaphr_integral_r4
           !dir$ attributes code_align : 32 :: rho_diaphr_integral_r4
           real(kind=sp),      intent(in) :: p2phi
           real(kind=sp),      intent(in) :: p1phi
           real(kind=sp),      intent(in) :: rhophi
           real(kind=sp),      intent(in) :: sigma
           real(kind=sp) :: rhod
           real(kind=sp), automatic :: t0,t1,t2,t3
           t0   = p2phi*p2phi
           t1   = p1phi*p1phi
           t2   = 2.0_sp/sigma
           t3   = t0-t1
           rhod = t3*rhophi*t2     
       end function rho_diaphr_integral_r4                                          
     

       pure elemental function rho_diaphr_integral_r8(p2phi,p1phi,rhophi,sigma) result(rhod)
           !dir$ optimize:3
           !dir$ attributes forceinline :: rho_diaphr_integral_r8
           !dir$ attributes code_align : 32 :: rho_diaphr_integral_r8
           real(kind=dp),      intent(in) :: p2phi
           real(kind=dp),      intent(in) :: p1phi
           real(kind=dp),      intent(in) :: rhophi
           real(kind=dp),      intent(in) :: sigma
           real(kind=dp) :: rhod
           real(kind=dp), automatic :: t0,t1,t2,t3
           t0   = p2phi*p2phi
           t1   = p1phi*p1phi
           t2   = 2.0_dp/sigma
           t3   = t0-t1
           rhod = t3*rhophi*t2     
       end function rho_diaphr_integral_r8


       !Спектр падающего на растр потока излучения равен
       !Formula 3, p. 202 
       subroutine raster_flux_sinc_unroll_16x_r4(phi0f,htin,f,phi0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_sinc_unroll_16x_r4
           !dir$ attributes forceinline ::   raster_flux_sinc_unroll_16x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_flux_sinc_unroll_16x_r4
           real(kind=sp),   dimension(-htin:htin),  intent(out) :: phi0f
           integer(kind=i4),                   intent(in)  :: htin !shall be divisable by 2
           real(kind=sp),                      intent(in)  :: f
           real(kind=sp),                      intent(in)  :: phi0
           real(kind=sp), parameter :: pi = 3.14159265358979323846264338328_sp
           real(kind=sp), automatic :: sinc0,sinc1,sinc2,sinc3,sinc4,sinc5,sinc6,sinc7
           real(kind=sp), automatic :: sinc8,sinc9,sinc10,sinc11,sinc12,sinc13,sinc14,sinc15
           real(kind=sp), automatic :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8
           real(kind=sp), automatic :: arg9,arg10,arg11,arg12,arg13,arg14,arg15
           real(kind=sp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15
           integer(kind=i4) :: i,m,m1,tin,nhtin
           tin = htin*2
           nhtin = -htin
           m = n(tin,16)
           if(m /= 0) then
              do i=1, m
                 t0       = real(i,kind=sp)
                 arg0     = pi*f*t0
                 sinc0    = sin(arg0)/arg0
                 phi0f(i) = sinc0 
              end do
              if(tin<16) return
           end if
           m1 = m+1
           ! Negative half first
           !dir$ assume_aligned phi0f:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=nhtin,m,-16
              t0         = real(i,kind=sp)
              arg0       = pi*f*t0
              sinc0      = sin(arg0)/arg0
              phi0f(i)   = sinc0
              t1         = real(i-1,kind=sp)
              arg1       = pi*f*t1
              sinc1      = sin(arg1)/arg1
              phi0f(i-1) = sinc1
              t2         = real(i-2,kind=sp)
              arg2       = pi*f*t2
              sinc2      = sin(arg2)/arg2
              phi0f(i-2) = sinc2
              t3         = real(i-3,kind=sp)
              arg3       = pi*f*t3
              sinc3      = sin(arg3)/arg3
              phi0f(i-3) = sinc3
              t4         = real(i-4,kind=sp)
              arg4       = pi*f*t4
              sinc4      = sin(arg4)/arg4
              phi0f(i-4) = sinc4 
              t5         = real(i-5,kind=sp)
              arg5       = pi*f*t5
              sinc5      = sin(arg5)/arg0 5
              phi0f(i-5) = sinc5
              t6         = real(i-6,kind=sp)
              arg6       = pi*f*t6
              sinc6      = sin(arg6)/arg6
              phi0f(i-6) = sinc6
              t7         = real(i-7,kind=sp)
              arg7       = pi*f*t7
              sinc7      = sin(arg7)/arg7
              phi0f(i-7) = sinc7
              t8         = real(i-8,kind=sp)
              arg8       = pi*f*t8
              sinc8      = sin(arg8)/arg8
              phi0f(i-8) = sinc8
              t9         = real(i-9,kind=sp)
              arg9       = pi*f*t9
              sinc9      = sin(arg9)/arg9
              phi0f(i-9) = sinc9
              t10        = real(i-10,kind=sp)
              arg10      = pi*f*t10
              sinc10     = sin(arg10)/arg10
              phi0f(i-10)= sinc10
              t11        = real(i-11,kind=sp)
              arg11      = pi*f*t11
              sinc11     = sin(arg11)/arg11
              phi0f(i-11)= sinc11
              t12        = real(i-12,kind=sp)
              arg12      = pi*f*t12
              sinc0      = sin(arg12)/arg12
              phi0f(i-12)= sinc12
              t13        = real(i-13,kind=sp)
              arg13      = pi*f*t13
              sinc13     = sin(arg13)/arg13
              phi0f(i-13)= sinc13
              t14        = real(i-14,kind=sp)
              arg14      = pi*f*t14
              sinc14     = sin(arg14)/arg14
              phi0f(i-14)= sinc14
              t15        = real(i-15,kind=sp)
              arg15      = pi*f*t15
              sinc15     = sin(arg15)/arg15
              phi0f(i-15)= sinc15
           end do
           !dir$ assume_aligned phi0f:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m,htin,16
              t0         = real(i,kind=sp)
              arg0       = pi*f*t0
              sinc0      = sin(arg0)/arg0
              phi0f(i)   = sinc0
              t1         = real(i+1,kind=sp)
              arg1       = pi*f*t1
              sinc1      = sin(arg1)/arg1
              phi0f(i+1) = sinc1
              t2         = real(i+2,kind=sp)
              arg2       = pi*f*t2
              sinc2      = sin(arg2)/arg2
              phi0f(i+2) = sinc2
              t3         = real(i+3,kind=sp)
              arg3       = pi*f*t3
              sinc3      = sin(arg3)/arg3
              phi0f(i+3) = sinc3
              t4         = real(i+4,kind=sp)
              arg4       = pi*f*t4
              sinc4      = sin(arg4)/arg4
              phi0f(i+4) = sinc4 
              t5         = real(i+5,kind=sp)
              arg5       = pi*f*t5
              sinc5      = sin(arg5)/arg0 5
              phi0f(i+5) = sinc5
              t6         = real(i+6,kind=sp)
              arg6       = pi*f*t6
              sinc6      = sin(arg6)/arg6
              phi0f(i+6) = sinc6
              t7         = real(i+7,kind=sp)
              arg7       = pi*f*t7
              sinc7      = sin(arg7)/arg7
              phi0f(i+7) = sinc7
              t8         = real(i+8,kind=sp)
              arg8       = pi*f*t8
              sinc8      = sin(arg8)/arg8
              phi0f(i+8) = sinc8
              t9         = real(i+9,kind=sp)
              arg9       = pi*f*t9
              sinc9      = sin(arg9)/arg9
              phi0f(i+9) = sinc9
              t10        = real(i+10,kind=sp)
              arg10      = pi*f*t10
              sinc10     = sin(arg10)/arg10
              phi0f(i+10)= sinc10
              t11        = real(i+11,kind=sp)
              arg11      = pi*f*t11
              sinc11     = sin(arg11)/arg11
              phi0f(i+11)= sinc11
              t12        = real(i+12,kind=sp)
              arg12      = pi*f*t12
              sinc0      = sin(arg12)/arg12
              phi0f(i+12)= sinc12
              t13        = real(i+13,kind=sp)
              arg13      = pi*f*t13
              sinc13     = sin(arg13)/arg13
              phi0f(i+13)= sinc13
              t14        = real(i+14,kind=sp)
              arg14      = pi*f*t14
              sinc14     = sin(arg14)/arg14
              phi0f(i+14)= sinc14
              t15        = real(i+15,kind=sp)
              arg15      = pi*f*t15
              sinc15     = sin(arg15)/arg15
              phi0f(i+15)= sinc15
           end do
       end subroutine raster_flux_sinc_unroll_16x_r4


       subroutine raster_flux_sinc_unroll_16x_r8(phi0f,htin,f,phi0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_sinc_unroll_16x_r8
           !dir$ attributes forceinline ::   raster_flux_sinc_unroll_16x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_flux_sinc_unroll_16x_r8
           real(kind=dp),   dimension(-htin:htin),  intent(out) :: phi0f
           integer(kind=i4),                   intent(in)  :: htin !shall be divisable by 2
           real(kind=dp),                      intent(in)  :: f
           real(kind=dp),                      intent(in)  :: phi0
           real(kind=dp), parameter :: pi = 3.14159265358979323846264338328_dp
           real(kind=dp), automatic :: sinc0,sinc1,sinc2,sinc3,sinc4,sinc5,sinc6,sinc7
           real(kind=dp), automatic :: sinc8,sinc9,sinc10,sinc11,sinc12,sinc13,sinc14,sinc15
           real(kind=dp), automatic :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8
           real(kind=dp), automatic :: arg9,arg10,arg11,arg12,arg13,arg14,arg15
           real(kind=dp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15
           integer(kind=i4) :: i,m,m1,tin,nhtin
           tin = htin*2
           nhtin = -htin
           m = n(tin,16)
           if(m /= 0) then
              do i=1, m
                 t0       = real(i,kind=dp)
                 arg0     = pi*f*t0
                 sinc0    = sin(arg0)/arg0
                 phi0f(i) = phi*t0*sinc0 
              end do
              if(tin<16) return
           end if
           m1 = m+1
           ! Negative half first
           !dir$ assume_aligned phi0f:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=nhtin,m,-16
              t0         = real(i,kind=dp)
              arg0       = pi*f*t0
              sinc0      = sin(arg0)/arg0
              phi0f(i)   = phi0*t0*sinc0
              t1         = real(i-1,kind=dp)
              arg1       = pi*f*t1
              sinc1      = sin(arg1)/arg1
              phi0f(i-1) = phi0*t1*sinc1
              t2         = real(i-2,kind=dp)
              arg2       = pi*f*t2
              sinc2      = sin(arg2)/arg2
              phi0f(i-2) = phi0*t2*sinc2
              t3         = real(i-3,kind=dp)
              arg3       = pi*f*t3
              sinc3      = sin(arg3)/arg3
              phi0f(i-3) = phi0*t3*sinc3
              t4         = real(i-4,kind=dp)
              arg4       = pi*f*t4
              sinc4      = sin(arg4)/arg4
              phi0f(i-4) = phi0*t4*sinc4 
              t5         = real(i-5,kind=dp)
              arg5       = pi*f*t5
              sinc5      = sin(arg5)/arg5
              phi0f(i-5) = phi0*t5*sinc5
              t6         = real(i-6,kind=dp)
              arg6       = pi*f*t6
              sinc6      = sin(arg6)/arg6
              phi0f(i-6) = phi0*t6*sinc6
              t7         = real(i-7,kind=dp)
              arg7       = pi*f*t7
              sinc7      = sin(arg7)/arg7
              phi0f(i-7) = phi0*t7*sinc7
              t8         = real(i-8,kind=dp)
              arg8       = pi*f*t8
              sinc8      = sin(arg8)/arg8
              phi0f(i-8) = phi0*t8*sinc8
              t9         = real(i-9,kind=dp)
              arg9       = pi*f*t9
              sinc9      = sin(arg9)/arg9
              phi0f(i-9) = phi0*t9*sinc9
              t10        = real(i-10,kind=dp)
              arg10      = pi*f*t10
              sinc10     = sin(arg10)/arg10
              phi0f(i-10)= phi0*t10*sinc10
              t11        = real(i-11,kind=dp)
              arg11      = pi*f*t11
              sinc11     = sin(arg11)/arg11
              phi0f(i-11)= phi0*t11*sinc11
              t12        = real(i-12,kind=dp)
              arg12      = pi*f*t12
              sinc0      = sin(arg12)/arg12
              phi0f(i-12)= phi0*t12*sinc12
              t13        = real(i-13,kind=dp)
              arg13      = pi*f*t13
              sinc13     = sin(arg13)/arg13
              phi0f(i-13)= phi0*t13*sinc13
              t14        = real(i-14,kind=dp)
              arg14      = pi*f*t14
              sinc14     = sin(arg14)/arg14
              phi0f(i-14)= phi0*t13*sinc14
              t15        = real(i-15,kind=dp)
              arg15      = pi*f*t15
              sinc15     = sin(arg15)/arg15
              phi0f(i-15)= phi0*t14*sinc15
           end do
           !dir$ assume_aligned phi0f:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m,htin,16
              t0         = real(i,kind=dp)
              arg0       = twopi*f*t0
              sinc0      = sin(arg0)/arg0
              phi0f(i)   = phi0*t0*sinc0
              t1         = real(i+1,kind=dp)
              arg1       = pi*f*t1
              sinc1      = sin(arg1)/arg1
              phi0f(i+1) = phi0*t1*sinc1
              t2         = real(i+2,kind=dp)
              arg2       = pi*f*t2
              sinc2      = sin(arg2)/arg2
              phi0f(i+2) = phi0*t2*sinc2
              t3         = real(i+3,kind=dp)
              arg3       = pi*f*t3
              sinc3      = sin(arg3)/arg3
              phi0f(i+3) = phi0*t3*sinc3
              t4         = real(i+4,kind=dp)
              arg4       = pi*f*t4
              sinc4      = sin(arg4)/arg4
              phi0f(i+4) = phi0*t4*sinc4 
              t5         = real(i+5,kind=dp)
              arg5       = pi*f*t5
              sinc5      = sin(arg5)/arg5 
              phi0f(i+5) = phi0*t5*sinc5
              t6         = real(i+6,kind=dp)
              arg6       = pi*f*t6
              sinc6      = sin(arg6)/arg6
              phi0f(i+6) = phi0*t6*sinc6
              t7         = real(i+7,kind=dp)
              arg7       = pi*f*t7
              sinc7      = sin(arg7)/arg7
              phi0f(i+7) = phi0*t7*sinc7
              t8         = real(i+8,kind=dp)
              arg8       = pi*f*t8
              sinc8      = sin(arg8)/arg8
              phi0f(i+8) = phi0*t8*sinc8
              t9         = real(i+9,kind=dp)
              arg9       = pi*f*t9
              sinc9      = sin(arg9)/arg9
              phi0f(i+9) = phi0*t9*sinc9
              t10        = real(i+10,kind=dp)
              arg10      = pi*f*t10
              sinc10     = sin(arg10)/arg10
              phi0f(i+10)= phi0*t10*sinc10
              t11        = real(i+11,kind=dp)
              arg11      = pi*f*t11
              sinc11     = sin(arg11)/arg11
              phi0f(i+11)= phi0*t11*sinc11
              t12        = real(i+12,kind=dp)
              arg12      = pi*f*t12
              sinc0      = sin(arg12)/arg12
              phi0f(i+12)= phi0*t12*sinc12
              t13        = real(i+13,kind=dp)
              arg13      = pi*f*t13
              sinc13     = sin(arg13)/arg13
              phi0f(i+13)= phi0*t13*sinc13
              t14        = real(i+14,kind=dp)
              arg14      = pi*f*t14
              sinc14     = sin(arg14)/arg14
              phi0f(i+14)= phi0*t14*sinc14
              t15        = real(i+15,kind=dp)
              arg15      = pi*f*t15
              sinc15     = sin(arg15)/arg15
              phi0f(i+15)= phi0*t15*sinc15
           end do
       end subroutine raster_flux_sinc_unroll_16x_r8


       subroutine raster_flux_sinc_unroll_8x_r4(phi0f,htin,f,phi0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_sinc_unroll_8x_r4
           !dir$ attributes forceinline ::   raster_flux_sinc_unroll_8x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_flux_sinc_unroll_8x_r4
           real(kind=sp),   dimension(-htin:htin),  intent(out) :: phi0f
           integer(kind=i4),                   intent(in)  :: htin !shall be divisable by 2
           real(kind=sp),                      intent(in)  :: f
           real(kind=sp),                      intent(in)  :: phi0
           real(kind=sp), parameter :: pi = 3.14159265358979323846264338328_sp
           real(kind=sp), automatic :: sinc0,sinc1,sinc2,sinc3,sinc4,sinc5,sinc6,sinc7
           real(kind=sp), automatic :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8
           real(kind=sp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7
           integer(kind=i4) :: i,m,m1,tin,nhtin,fac
           tin = htin*2
           fac = real(tin,kind=sp)*phi0
           nhtin = -htin
           m = n(tin,8)
           if(m /= 0) then
              do i=1, m
                 t0       = real(i,kind=sp)
                 arg0     = pi*f*t0
                 sinc0    = sin(arg0)/arg0
                 phi0f(i) = phi0*t0*sinc0 
              end do
              if(tin<8) return
           end if
           m1 = m+1
           ! Negative half first
           !dir$ assume_aligned phi0f:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=nhtin,m,-8
              t0         = real(i,kind=sp)
              arg0       = pi*f*t0
              sinc0      = sin(arg0)/arg0
              phi0f(i)   = phi0*t0*sinc0
              t1         = real(i-1,kind=sp)
              arg1       = pi*f*t1
              sinc1      = sin(arg1)/arg1
              phi0f(i-1) = phi0*t1*sinc1
              t2         = real(i-2,kind=sp)
              arg2       = pi*f*t2
              sinc2      = sin(arg2)/arg2
              phi0f(i-2) = phi0*t2*sinc2
              t3         = real(i-3,kind=sp)
              arg3       = pi*f*t3
              sinc3      = sin(arg3)/arg3
              phi0f(i-3) = phi0*t3*sinc3
              t4         = real(i-4,kind=sp)
              arg4       = pi*f*t4
              sinc4      = sin(arg4)/arg4
              phi0f(i-4) = phi0*t4*sinc4 
              t5         = real(i-5,kind=sp)
              arg5       = pi*f*t5
              sinc5      = sin(arg5)/arg5
              phi0f(i-5) = phi0*t5*sinc5
              t6         = real(i-6,kind=sp)
              arg6       = pi*f*t6
              sinc6      = sin(arg6)/arg6
              phi0f(i-6) = phi0*t6*sinc6
              t7         = real(i-7,kind=sp)
              arg7       = pi*f*t7
              sinc7      = sin(arg7)/arg7
              phi0f(i-7) = phi0*t7*sinc7
           end do
           !dir$ assume_aligned phi0f:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m,htin,8
              t0         = real(i,kind=sp)
              arg0       = pi*f*t0
              sinc0      = sin(arg0)/arg0
              phi0f(i)   = phi0*t0*sinc0
              t1         = real(i+1,kind=sp)
              arg1       = pi*f*t1
              sinc1      = sin(arg1)/arg1
              phi0f(i+1) = phi0*t1*sinc1
              t2         = real(i+2,kind=sp)
              arg2       = pi*f*t2
              sinc2      = sin(arg2)/arg2
              phi0f(i+2) = phi0*t2*sinc2
              t3         = real(i+3,kind=sp)
              arg3       = pi*f*t3
              sinc3      = sin(arg3)/arg3
              phi0f(i+3) = phi0*t3*sinc3
              t4         = real(i+4,kind=sp)
              arg4       = pi*f*t4
              sinc4      = sin(arg4)/arg4
              phi0f(i+4) = phi0*t4*sinc4 
              t5         = real(i+5,kind=sp)
              arg5       = pi*f*t5
              sinc5      = sin(arg5)/arg5 
              phi0f(i+5) = phi0*t5*sinc5
              t6         = real(i+6,kind=sp)
              arg6       = pi*f*t6
              sinc6      = sin(arg6)/arg6
              phi0f(i+6) = phi0*t6*sinc6
              t7         = real(i+7,kind=sp)
              arg7       = pi*f*t7
              sinc7      = sin(arg7)/arg7
              phi0f(i+7) = phi0*t7*sinc7
           end do
       end subroutine raster_flux_sinc_unroll_8x_r4


       subroutine raster_flux_sinc_unroll_8x_r8(phi0f,htin,f,phi0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_sinc_unroll_8x_r8
           !dir$ attributes forceinline ::   raster_flux_sinc_unroll_8x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_flux_sinc_unroll_8x_r8
           real(kind=dp),   dimension(-htin:htin),  intent(out) :: phi0f
           integer(kind=i4),                   intent(in)  :: htin !shall be divisable by 2
           real(kind=dp),                      intent(in)  :: f
           real(kind=dp),                      intent(in)  :: phi0
           real(kind=dp), parameter :: pi = 3.14159265358979323846264338328_dp
           real(kind=dp), automatic :: sinc0,sinc1,sinc2,sinc3,sinc4,sinc5,sinc6,sinc7
           real(kind=dp), automatic :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8
           real(kind=dp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7
           integer(kind=i4) :: i,m,m1,tin,nhtin
           tin = htin*2
           nhtin = -htin
           m = n(tin,8)
           if(m /= 0) then
              do i=1, m
                 t0       = real(i,kind=dp)
                 arg0     = pi*f*t0
                 sinc0    = sin(arg0)/arg0
                 phi0f(i) = phi0*t0*sinc0 
              end do
              if(tin<8) return
           end if
           m1 = m+1
           ! Negative half first
           !dir$ assume_aligned phi0f:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=nhtin,m,-8
              t0         = real(i,kind=dp)
              arg0       = pi*f*t0
              sinc0      = sin(arg0)/arg0
              phi0f(i)   = phi0*t0*sinc0
              t1         = real(i-1,kind=dp)
              arg1       = pi*f*t1
              sinc1      = sin(arg1)/arg1
              phi0f(i-1) = phi0*t1*sinc1
              t2         = real(i-2,kind=dp)
              arg2       = pi*f*t2
              sinc2      = sin(arg2)/arg2
              phi0f(i-2) = phi0*t2*sinc2
              t3         = real(i-3,kind=dp)
              arg3       = pi*f*t3
              sinc3      = sin(arg3)/arg3
              phi0f(i-3) = phi0*t3*sinc3
              t4         = real(i-4,kind=dp)
              arg4       = pi*f*t4
              sinc4      = sin(arg4)/arg4
              phi0f(i-4) = phi0*t4*sinc4 
              t5         = real(i-5,kind=dp)
              arg5       = pi*f*t5
              sinc5      = sin(arg5)/arg5 
              phi0f(i-5) = phi0*t5*sinc5
              t6         = real(i-6,kind=dp)
              arg6       = pi*f*t6
              sinc6      = sin(arg6)/arg6
              phi0f(i-6) = phi0*t6*sinc6
              t7         = real(i-7,kind=dp)
              arg7       = pi*f*t7
              sinc7      = sin(arg7)/arg7
              phi0f(i-7) = phi0*t7*sinc7
           end do
           !dir$ assume_aligned phi0f:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m,htin,8
              t0         = real(i,kind=dp)
              arg0       = pi*f*t0
              sinc0      = sin(arg0)/arg0
              phi0f(i)   = phi0*t0*sinc0
              t1         = real(i+1,kind=dp)
              arg1       = pi*f*t1
              sinc1      = sin(arg1)/arg1
              phi0f(i+1) = phi0*t1*sinc1
              t2         = real(i+2,kind=dp)
              arg2       = pi*f*t2
              sinc2      = sin(arg2)/arg2
              phi0f(i+2) = phi0*t2*sinc2
              t3         = real(i+3,kind=dp)
              arg3       = pi*f*t3
              sinc3      = sin(arg3)/arg3
              phi0f(i+3) = phi0*t3*sinc3
              t4         = real(i+4,kind=dp)
              arg4       = pi*f*t4
              sinc4      = sin(arg4)/arg4
              phi0f(i+4) = phi0*t4*sinc4 
              t5         = real(i+5,kind=dp)
              arg5       = pi*f*t5
              sinc5      = sin(arg5)/arg5 
              phi0f(i+5) = phi0*t5*sinc5
              t6         = real(i+6,kind=dp)
              arg6       = pi*f*t6
              sinc6      = sin(arg6)/arg6
              phi0f(i+6) = phi0*t6*sinc6
              t7         = real(i+7,kind=dp)
              arg7       = pi*f*t7
              sinc7      = sin(arg7)/arg7
              phi0f(i+7) = phi0*t7*sinc7
           end do
       end subroutine raster_flux_sinc_unroll_8x_r8


       subroutine raster_flux_sinc_unroll_4x_r4(phi0f,htin,f,phi0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_sinc_unroll_4x_r4
           !dir$ attributes forceinline ::   raster_flux_sinc_unroll_4x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_flux_sinc_unroll_4x_r4
           real(kind=sp),   dimension(-htin:htin),  intent(out) :: phi0f
           integer(kind=i4),                   intent(in)  :: htin !shall be divisable by 2
           real(kind=sp),                      intent(in)  :: f
           real(kind=sp),                      intent(in)  :: phi0
           real(kind=sp), parameter :: pi = 3.14159265358979323846264338328_sp
           real(kind=sp), automatic :: sinc0,sinc1,sinc2,sinc3
           real(kind=sp), automatic :: arg0,arg1,arg2,arg3
           real(kind=sp), automatic :: t0,t1,t2,t3
           integer(kind=i4) :: i,m,m1,tin,nhtin
           tin = htin*2
           nhtin = -htin
           m = n(tin,4)
           if(m /= 0) then
              do i=1, m
                 t0       = real(i,kind=sp)
                 arg0     = pi*f*t0
                 sinc0    = sin(arg0)/arg0
                 phi0f(i) = phi0*t0*sinc0 
              end do
              if(tin<4) return
           end if
           m1 = m+1
           ! Negative half first
           !dir$ assume_aligned phi0f:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=nhtin,m,-4
              t0         = real(i,kind=sp)
              arg0       = pi*f*t0
              sinc0      = sin(arg0)/arg0
              phi0f(i)   = phi0*t0*sinc0
              t1         = real(i-1,kind=sp)
              arg1       = pi*f*t1
              sinc1      = sin(arg1)/arg1
              phi0f(i-1) = phi0*t1*sinc1
              t2         = real(i-2,kind=sp)
              arg2       = pi*f*t2
              sinc2      = sin(arg2)/arg2
              phi0f(i-2) = phi0*t2*sinc2
              t3         = real(i-3,kind=sp)
              arg3       = pi*f*t3
              sinc3      = sin(arg3)/arg3
              phi0f(i-3) = phi0*t3*sinc3
           end do
           !dir$ assume_aligned phi0f:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m,htin,4
              t0         = real(i,kind=sp)
              arg0       = pi*f*t0
              sinc0      = sin(arg0)/arg0
              phi0f(i)   = phi0*t0*sinc0
              t1         = real(i+1,kind=sp)
              arg1       = pi*f*t1
              sinc1      = sin(arg1)/arg1
              phi0f(i+1) = phi0*t1*sinc1
              t2         = real(i+2,kind=sp)
              arg2       = pi*f*t2
              sinc2      = sin(arg2)/arg2
              phi0f(i+2) = phi0*t2*sinc2
              t3         = real(i+3,kind=sp)
              arg3       = pi*f*t3
              sinc3      = sin(arg3)/arg3
              phi0f(i+3) = phi0*t3*sinc3
           end do
       end subroutine raster_flux_sinc_unroll_4x_r4


       subroutine raster_flux_sinc_unroll_4x_r8(phi0f,htin,f,phi0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_sinc_unroll_4x_r8
           !dir$ attributes forceinline ::   raster_flux_sinc_unroll_4x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_flux_sinc_unroll_4x_r8
           real(kind=dp),   dimension(-htin:htin),  intent(out) :: phi0f
           integer(kind=i4),                   intent(in)  :: htin !shall be divisable by 2
           real(kind=dp),                      intent(in)  :: f
           real(kind=dp),                      intent(in)  :: phi0
           real(kind=dp), parameter :: pi = 3.14159265358979323846264338328_dp
           real(kind=dp), automatic :: sinc0,sinc1,sinc2,sinc3
           real(kind=dp), automatic :: arg0,arg1,arg2,arg3
           real(kind=dp), automatic :: t0,t1,t2,t3
           integer(kind=i4) :: i,m,m1,tin,nhtin
           tin = htin*2
           nhtin = -htin
           m = n(tin,4)
           if(m /= 0) then
              do i=1, m
                 t0       = real(i,kind=dp)
                 arg0     = pi*f*t0
                 sinc0    = sin(arg0)/arg0
                 phi0f(i) = phi0*t0*sinc0 
              end do
              if(tin<4) return
           end if
           m1 = m+1
           ! Negative half first
           !dir$ assume_aligned phi0f:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=nhtin,m,-4
              t0         = real(i,kind=dp)
              arg0       = pi*f*t0
              sinc0      = sin(arg0)/arg0
              phi0f(i)   = phi0*t0*sinc0
              t1         = real(i-1,kind=dp)
              arg1       = pi*f*t1
              sinc1      = sin(arg1)/arg1
              phi0f(i-1) = phi0*t1*sinc1
              t2         = real(i-2,kind=dp)
              arg2       = pi*f*t2
              sinc2      = sin(arg2)/arg2
              phi0f(i-2) = phi0*t2*sinc2
              t3         = real(i-3,kind=dp)
              arg3       = pi*f*t3
              sinc3      = sin(arg3)/arg3
              phi0f(i-3) = phi0*t3*sinc3
           end do
           !dir$ assume_aligned phi0f:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m,htin,4
              t0         = real(i,kind=dp)
              arg0       = pi*f*t0
              sinc0      = sin(arg0)/arg0
              phi0f(i)   = phi0*t0*sinc0
              t1         = real(i+1,kind=dp)
              arg1       = pi*f*t1
              sinc1      = sin(arg1)/arg1
              phi0f(i+1) = phi0*t1*sinc1
              t2         = real(i+2,kind=dp)
              arg2       = pi*f*t2
              sinc2      = sin(arg2)/arg2
              phi0f(i+2) = phi0*t2*sinc2
              t3         = real(i+3,kind=dp)
              arg3       = pi*f*t3
              sinc3      = sin(arg3)/arg3
              phi0f(i+3) = phi0*t3*sinc3
           
           end do
       end subroutine raster_flux_sinc_unroll_4x_r8


       subroutine raster_flux_sinc_unroll_2x_r4(phi0f,htin,f,phi0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_sinc_unroll_2x_r4
           !dir$ attributes forceinline ::   raster_flux_sinc_unroll_2x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_flux_sinc_unroll_2x_r4
           real(kind=sp),   dimension(-htin:htin),  intent(out) :: phi0f
           integer(kind=i4),                   intent(in)  :: htin !shall be divisable by 2
           real(kind=sp),                      intent(in)  :: f
           real(kind=sp),                      intent(in)  :: phi0
           real(kind=sp), parameter :: pi = 3.14159265358979323846264338328_sp
           real(kind=sp), automatic :: sinc0,sinc1
           real(kind=sp), automatic :: arg0,arg1
           real(kind=sp), automatic :: t0,t1
           integer(kind=i4) :: i,m,m1,tin,nhtin
           tin = htin*2
           nhtin = -htin
           m = n(tin,2)
           if(m /= 0) then
              do i=1, m
                 t0       = real(i,kind=sp)
                 arg0     = pi*f*t0
                 sinc0    = sin(arg0)/arg0
                 phi0f(i) = phi0*t0*sinc0 
              end do
              if(tin<2) return
           end if
           m1 = m+1
           ! Negative half first
           !dir$ assume_aligned phi0f:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=nhtin,m,-2
              t0         = real(i,kind=sp)
              arg0       = pi*f*t0
              sinc0      = sin(arg0)/arg0
              phi0f(i)   = phi0*t0*sinc0
              t1         = real(i-1,kind=sp)
              arg1       = pi*f*t1
              sinc1      = sin(arg1)/arg1
              phi0f(i-1) = phi0*t0*sinc1
           end do
           !dir$ assume_aligned phi0f:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m,htin,2
              t0         = real(i,kind=sp)
              arg0       = pi*f*t0
              sinc0      = sin(arg0)/arg0
              phi0f(i)   = phi0*t0*sinc0
              t1         = real(i+1,kind=sp)
              arg1       = pi*f*t1
              sinc1      = sin(arg1)/arg1
              phi0f(i+1) = phi0*t1*sinc1
             
           end do
       end subroutine raster_flux_sinc_unroll_2x_r4


       subroutine raster_flux_sinc_unroll_2x_r8(phi0f,htin,f,phi0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_sinc_unroll_2x_r8
           !dir$ attributes forceinline ::   raster_flux_sinc_unroll_2x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_flux_sinc_unroll_2x_r8
           real(kind=dp),   dimension(-htin:htin),  intent(out) :: phi0f
           integer(kind=i4),                   intent(in)  :: htin !shall be divisable by 2
           real(kind=dp),                      intent(in)  :: f
           real(kind=dp),                      intent(in)  :: phi0
           real(kind=dp), parameter :: pi = 3.14159265358979323846264338328_dp
           real(kind=dp), automatic :: sinc0,sinc1
           real(kind=dp), automatic :: arg0,arg1
           real(kind=dp), automatic :: t0,t1
           integer(kind=i4) :: i,m,m1,tin,nhtin
           tin = htin*2
           nhtin = -htin
           m = n(tin,2)
           if(m /= 0) then
              do i=1, m
                 t0       = real(i,kind=dp)
                 arg0     = pi*f*t0
                 sinc0    = sin(arg0)/arg0
                 phi0f(i) = phi0*t0*sinc0 
              end do
              if(tin<2) return
           end if
           m1 = m+1
           ! Negative half first
           !dir$ assume_aligned phi0f:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=nhtin,m,-2
              t0         = real(i,kind=dp)
              arg0       = pi*f*t0
              sinc0      = sin(arg0)/arg0
              phi0f(i)   = phi0*t0*sinc0
              t1         = real(i-1,kind=dp)
              arg1       = pi*f*t1
              sinc1      = sin(arg1)/arg1
              phi0f(i-1) = phi0*t1*sinc1
           end do
           !dir$ assume_aligned phi0f:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=m,htin,2
              t0         = real(i,kind=dp)
              arg0       = pi*f*t0
              sinc0      = sin(arg0)/arg0
              phi0f(i)   = phi0*t0*sinc0
              t1         = real(i+1,kind=dp)
              arg1       = pi*f*t1
              sinc1      = sin(arg1)/arg1
              phi0f(i+1) = phi0*t1*sinc1
                      
           end do
       end subroutine raster_flux_sinc_unroll_2x_r8

       
       subroutine raster_flux_sinc_r4(phi0f,htin,f,phi0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_sinc_r4
           !dir$ attributes forceinline ::   raster_flux_sinc_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_flux_sinc_r4
           real(kind=sp),   dimension(-htin:htin),  intent(out) :: phi0f
           integer(kind=i4),                   intent(in)  :: htin !shall be divisable by 2
           real(kind=sp),                      intent(in)  :: f
           real(kind=sp),                      intent(in)  :: phi0
           real(kind=sp), parameter :: pi = 3.14159265358979323846264338328_sp
           real(kind=sp), automatic :: sinc0
           real(kind=sp), automatic :: arg0
           real(kind=sp), automatic :: t0
           integer(kind=i4) :: i,tin,nhtin
           tin = htin*2
           nhtin = -htin
                   
           ! Negative half first
           !dir$ assume_aligned phi0f:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=nhtin,0
              t0         = real(i,kind=sp)
              arg0       = pi*f*t0
              sinc0      = sin(arg0)/arg0
              phi0f(i)   = phi0*t0*sinc0
           end do
           !dir$ assume_aligned phi0f:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=1,htin
              t0         = real(i,kind=sp)
              arg0       = pi*f*t0
              sinc0      = sin(arg0)/arg0
              phi0f(i)   = phi0*t0*sinc0
                         
           end do
       end subroutine raster_flux_sinc_r4


       subroutine raster_flux_sinc_r8(phi0f,htin,f,phi0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_sinc_r8
           !dir$ attributes forceinline ::   raster_flux_sinc_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_flux_sinc_r8
           real(kind=dp),   dimension(-htin:htin),  intent(out) :: phi0f
           integer(kind=i4),                   intent(in)  :: htin !shall be divisable by 2
           real(kind=dp),                      intent(in)  :: f
           real(kind=dp),                      intent(in)  :: phi0
           real(kind=dp), parameter :: pi = 3.14159265358979323846264338328_dp
           real(kind=dp), automatic :: sinc0
           real(kind=dp), automatic :: arg0
           real(kind=dp), automatic :: t0
           integer(kind=i4) :: i,tin,nhtin
           tin = htin*2
           nhtin = -htin
                   
           ! Negative half first
           !dir$ assume_aligned phi0f:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=nhtin,0
              t0         = real(i,kind=dp)
              arg0       = pi*f*t0
              sinc0      = sin(arg0)/arg0
              phi0f(i)   = phi0*t0*sinc0
           end do
           !dir$ assume_aligned phi0f:64
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector multiple_gather_scatter_by_shuffles 
           !dir$ vector always
           do i=1,htin
              t0         = real(i,kind=dp)
              arg0       = pi*f*t0
              sinc0      = sin(arg0)/arg0
              phi0f(i)   = phi0*t0*sinc0
                         
           end do
       end subroutine raster_flux_sinc_r8


       subroutine raster_flux_sinc_exec_r4(phi0f,htin,f,phi0,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_sinc_exec_r4
           real(kind=sp),   dimension(-htin:htin),  intent(out) :: phi0f
           integer(kind=i4),                   intent(in)  :: htin !shall be divisable by 2
           real(kind=sp),                      intent(in)  :: f
           real(kind=sp),                      intent(in)  :: phi0
           integer(kind=i4),                   intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                call raster_flux_sinc_unroll_16x_r4(phi0f,htin,f,phi0)
              case (8)
                call raster_flux_sinc_unroll_8x_r4(phi0f,htin,f,phi0)
              case (4)
                call raster_flux_sinc_unroll_4x_r4(phi0f,htin,f,phi0)
              case (2)
                call raster_flux_sinc_unroll_2x_r4(phi0f,htin,f,phi0) 
              case (0)
                call raster_flux_sinc_r4(phi0f,htin,f,phi0)
              case default
                return
           end select
       end subroutine raster_flux_sinc_exec_r4


       subroutine raster_flux_sinc_exec_r8(phi0f,htin,f,phi0,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_sinc_exec_r8
           real(kind=dp),   dimension(-htin:htin),  intent(out) :: phi0f
           integer(kind=i4),                   intent(in)  :: htin !shall be divisable by 2
           real(kind=dp),                      intent(in)  :: f
           real(kind=dp),                      intent(in)  :: phi0
           integer(kind=i4),                   intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                call raster_flux_sinc_unroll_16x_r8(phi0f,htin,f,phi0)
              case (8)
                call raster_flux_sinc_unroll_8x_r8(phi0f,htin,f,phi0)
              case (4)
                call raster_flux_sinc_unroll_4x_r8(phi0f,htin,f,phi0)
              case (2)
                call raster_flux_sinc_unroll_2x_r8(phi0f,htin,f,phi0) 
              case (0)
                call raster_flux_sinc_r8(phi0f,htin,f,phi0)
              case default
                return
           end select
       end subroutine raster_flux_sinc_exec_r8



       !Спектр модулированного потока излучения при максимальной
       !глубине модуляции, когда тх = т0 = 0,5, равен
       !Formula 3, p. 202
       subroutine raster_flux_mod_sinc_unroll_16x_r4(phif,htin,f,phi0,f0,rho0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_mod_sinc_unroll_16x_r4
           !dir$ attributes forceinline ::   raster_flux_mod_sinc_unroll_16x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_flux_mod_sinc_unroll_16x_r4
           real(kind=sp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=sp),                         intent(in)  :: f
           real(kind=sp),                         intent(in)  :: phi0
           real(kind=sp),                         intent(in)  :: f0
           real(kind=sp),                         intent(in)  :: rho0
           real(kind=sp), parameter :: pi = 3.14159265358979323846264338328_sp
           real(kind=sp), automatic :: sinc0,sinc1,sinc2,sinc3,sinc4,sinc5,sinc6,sinc7
           real(kind=sp), automatic :: sinc8,sinc9,sinc10,sinc11,sinc12,sinc13,sinc14,sinc15
           real(kind=sp), automatic :: sinc10,sinc11,sinc12,sinc13,sinc14,sinc15,sinc16,sinc17
           real(kind=sp), automatic :: sinc18,sinc19,sinc110,sinc111,sinc112,sinc113,sinc114,sinc115
           real(kind=sp), automatic :: sinc20,sinc21,sinc22,sinc23,sinc24,sinc25,sinc26,sinc27
           real(kind=sp), automatic :: sinc28,sinc29,sinc210,sinc211,sinc212,sinc213,sinc214,sinc215
           real(kind=sp), automatic :: a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15
           real(kind=sp), automatic :: b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15
           real(kind=sp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15
           real(kind=sp), automatic :: c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15
           real(kind=sp), automatic :: sum0,sum1,sum2,sum3,sum4,sum5,sum6,sum7
           real(kind=sp), automatic :: sum8,sum9,sum10,sum11,sum12,sum13,sum14,sum15
           real(kind=sp), automatic :: f0,f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15
           real(kind=sp), automatic :: g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15
           real(kind=sp), automatic :: fdif,fsum,phi02,phi04
           integer(kind=i4) :: i,m,m1,tin,nhtin
           tin = htin*2
           nhtin = -htin
           fdif  = f-f0
           fsum  = f+f0
           phi02 = 0.5_sp*phi0
           phi04 = 0.25_sp*phi0
           if(rho0/=0.5_sp) then
              call raster_flux_sinc_unroll_16x_r4(phif,htin,f,phi0)
              return
           else
              m = n(tin,16)
              if(m /= 0) then
                 do i=1,m
                    t0      = real(i,kind=sp)
                    a0      = pi*f*t0
                    f0      = phi02*t0
                    sinc0   = f0*sin(a0)/a0
                    b0      = pi*fdif*t0
                    sinc10  = sin(b0)/b0
                    c0      = pi*fsum*t0
                    g0      = phi04*t0
                    sinc20  = sin(c0)/c0
                    sum0    = g0*(sinc0+sinc20)
                   phif(i) = sinc0+sum0
                 end do
                 if(tin<16) return
              end if
              m1 = m+1
              ! Negative half first
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(4)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=nhtin,m,-16
                   t0        = real(i,kind=sp)
                   a0        = pi*f*t0
                   f0        = phi02*t0
                   sinc0     = f0*sin(a0)/a0
                   b0        = pi*fdif*t0
                   sinc10    = sin(b0)/b0
                   c0        = pi*fsum*t0
                   g0        = phi04*t0
                   sinc20    = sin(c0)/c0
                   sum0      = g0*(sinc0+sinc20)
                   phif(i)   = sinc0+sum0
                   t1        = real(i-1,kind=sp)
                   a1        = pi*f*t1
                   f1        = phi02*t1
                   sinc1     = f1*sin(a1)/a1
                   b1        = pi*fdif*t1
                   sinc11    = sin(b1)/b1
                   c1        = pi*fsum*t1
                   g1        = phi04*t1
                   sinc21    = sin(c1)/c1
                   sum1      = g1*(sinc1+sinc21)
                   phif(i-1) = sinc1+sum1
                   t2        = real(i-2,kind=sp)
                   a2        = pi*f*t2
                   f2        = phi02*t2
                   sinc2     = f2*sin(a2)/a2
                   b2        = pi*fdif*t2
                   sinc12    = sin(b2)/b2
                   c2        = pi*fsum*t2
                   g2        = phi04*t2
                   sinc22    = sin(c2)/c2
                   sum2      = g2*(sinc2+sinc22)
                   phif(i-2) = sinc2+sum2
                   t3        = real(i-3,kind=sp)
                   a3        = pi*f*t3
                   f3        = phi02*t3
                   sinc3     = f3*sin(a3)/a3
                   b3        = pi*fdif*t3
                   sinc13    = sin(b3)/b3
                   c3        = pi*fsum*t3
                   g3        = phi04*t3
                   sinc23    = sin(c3)/c3
                   sum3      = g3*(sinc3+sinc23)
                   phif(i-3) = sinc3+sum3
                   t4        = real(i-4,kind=sp)
                   a4        = pi*f*t4
                   f4        = phi02*t4
                   sinc4     = f4*sin(a4)/a4
                   b4        = pi*fdif*t4
                   sinc11    = sin(b1)/b1
                   c4        = pi*fsum*t4
                   g4        = phi04*t4
                   sinc24    = sin(c4)/c4
                   sum4      = g4*(sinc4+sinc24)
                   phif(i-4) = sinc4+sum4
                   t5        = real(i-5,kind=sp)
                   a5        = pi*f*t5
                   f5        = phi02*t5
                   sinc5     = f5*sin(a5)/a5
                   b5        = pi*fdif*t5
                   sinc15    = sin(b5)/b5
                   c5        = pi*fsum*t5
                   g5        = phi04*t5
                   sinc25    = sin(c5)/c5
                   sum5      = g1*(sinc5+sinc25)
                   phif(i-5) = sinc5+sum5
                   t6        = real(i-6,kind=sp)
                   a6        = pi*f*t6
                   f6        = phi02*t6
                   sinc6     = f6*sin(a6)/a6
                   b6        = pi*fdif*t1
                   sinc16    = sin(b6)/b6
                   c6        = pi*fsum*t6
                   g6        = phi04*t6
                   sinc26    = sin(c6)/c6
                   sum6      = g6*(sinc6+sinc26)
                   phif(i-6) = sinc6+sum6
                   t7        = real(i-7,kind=sp)
                   a7        = pi*f*t7
                   f7        = phi02*t7
                   sinc7     = f7*sin(a7)/a7
                   b7        = pi*fdif*t7
                   sinc17    = sin(b7)/b7
                   c7        = pi*fsum*t7
                   g7        = phi04*t7
                   sinc27    = sin(c7)/c7
                   sum7      = g7*(sinc7+sinc27)
                   phif(i-7) = sinc7+sum7
                   t8        = real(i-8,kind=sp)
                   a8        = pi*f*t8
                   f8        = phi02*t8
                   sinc8      = f8*sin(a8)/a8
                   b8         = pi*fdif*t8
                   sinc18     = sin(b8)/b8
                   c8         = pi*fsum*t8
                   g8         = phi04*t8
                   sinc28     = sin(c8)/c8
                   sum8       = g8*(sinc8+sinc28)
                   phif(i-8)  = sinc8+sum8
                   t9         = real(i-9,kind=sp)
                   a9         = pi*f*t9
                   f9         = phi02*t9
                   sinc9      = f9*sin(a9)/a9
                   b9         = pi*fdif*t9
                   sinc19     = sin(b9)/b9
                   c9         = pi*fsum*t9
                   g9         = phi04*t9
                   sinc29     = sin(c9)/c9
                   sum9       = g9*(sinc9+sinc29)
                   phif(i-9)  = sinc9+sum9
                   t10        = real(i-10,kind=sp)
                   a10        = pi*f*t10
                   f10        = phi02*t10
                   sinc10     = f1*sin(a10)/a10
                   b10        = pi*fdif*t10
                   sinc110    = sin(b10)/b10
                   c10        = pi*fsum*t10
                   g10        = phi04*t10
                   sinc210    = sin(c10)/c10
                   sum10      = g10*(sinc10+sinc210)
                   phif(i-10) = sinc10+sum10
                   t11        = real(i-11,kind=sp)
                   a11        = pi*f*t11
                   f11        = phi02*t11
                   sinc11     = f11*sin(a11)/a11
                   b11        = pi*fdif*t11
                   sinc111    = sin(b11)/b11
                   c11        = pi*fsum*t11
                   g11        = phi04*t11
                   sinc211    = sin(c11)/c11
                   sum11      = g11*(sinc11+sinc211)
                   phif(i-11) = sinc11+sum11
                   t12        = real(i-12,kind=sp)
                   a12        = pi*f*t12
                   f12        = phi02*t12
                   sinc12     = f12*sin(a12)/a12
                   b12        = pi*fdif*t12
                   sinc112    = sin(b12)/b12
                   c12        = pi*fsum*t12
                   g12        = phi04*t12
                   sinc212    = sin(c12)/c12
                   sum12      = g12*(sinc12+sinc212)
                   phif(i-12) = sinc12+sum12
                   t13        = real(i-13,kind=sp)
                   a13        = pi*f*t13
                   f13        = phi02*t13
                   sinc13     = f1*sin(a13)/a13
                   b13        = pi*fdif*t13
                   sinc113    = sin(b13)/b13
                   c13        = pi*fsum*t13
                   g13        = phi04*t13
                   sinc213    = sin(c13)/c13
                   sum13      = g13*(sinc13+sinc213)
                   phif(i-13) = sinc13+sum13
                   t14        = real(i-14,kind=sp)
                   a14        = pi*f*t14
                   f14        = phi02*t14
                   sinc14     = f1*sin(a14)/a14
                   b14        = pi*fdif*t14
                   sinc114    = sin(b14)/b14
                   c14        = pi*fsum*t14
                   g14        = phi04*t14
                   sinc214    = sin(c14)/c14
                   sum14      = g1*(sinc14+sinc214)
                   phif(i-14) = sinc14+sum14
                   t15        = real(i-15,kind=sp)
                   a15        = pi*f*t15
                   f15        = phi02*t15
                   sinc15     = f15*sin(a15)/a15
                   b15        = pi*fdif*t15
                   sinc115    = sin(b15)/b15
                   c15        = pi*fsum*t15
                   g15        = phi04*t15
                   sinc215    = sin(c15)/c15
                   sum15      = g15*(sinc15+sinc215)
                   phif(i-15) = sinc15+sum15 
               end do
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(4)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=m,htin,16
                   t0        = real(i,kind=sp)
                   a0        = pi*f*t0
                   f0        = phi02*t0
                   sinc0     = f0*sin(a0)/a0
                   b0        = pi*fdif*t0
                   sinc10    = sin(b0)/b0
                   c0        = pi*fsum*t0
                   g0        = phi04*t0
                   sinc20    = sin(c0)/c0
                   sum0      = g0*(sinc0+sinc20)
                   phif(i)   = sinc0+sum0
                   t1        = real(i+1,kind=sp)
                   a1        = pi*f*t1
                   f1        = phi02*t1
                   sinc1     = f1*sin(a1)/a1
                   b1        = pi*fdif*t1
                   sinc11    = sin(b1)/b1
                   c1        = pi*fsum*t1
                   g1        = phi04*t1
                   sinc21    = sin(c1)/c1
                   sum1      = g1*(sinc1+sinc21)
                   phif(i+1) = sinc1+sum1
                   t2        = real(i+2,kind=sp)
                   a2        = pi*f*t2
                   f2        = phi02*t2
                   sinc2     = f2*sin(a2)/a2
                   b2        = pi*fdif*t2
                   sinc12    = sin(b2)/b2
                   c2        = pi*fsum*t2
                   g2        = phi04*t2
                   sinc22    = sin(c2)/c2
                   sum2      = g2*(sinc2+sinc22)
                   phif(i+2) = sinc2+sum2
                   t3        = real(i+3,kind=sp)
                   a3        = pi*f*t3
                   f3        = phi02*t3
                   sinc3     = f3*sin(a3)/a3
                   b3        = pi*fdif*t3
                   sinc13    = sin(b3)/b3
                   c3        = pi*fsum*t3
                   g3        = phi04*t3
                   sinc23    = sin(c3)/c3
                   sum3      = g3*(sinc3+sinc23)
                   phif(i+3) = sinc3+sum3
                   t4        = real(i+4,kind=sp)
                   a4        = pi*f*t4
                   f4        = phi02*t4
                   sinc4     = f4*sin(a4)/a4
                   b4        = pi*fdif*t4
                   sinc11    = sin(b1)/b1
                   c4        = pi*fsum*t4
                   g4        = phi04*t4
                   sinc24    = sin(c4)/c4
                   sum4      = g4*(sinc4+sinc24)
                   phif(i+4) = sinc4+sum4
                   t5        = real(i+5,kind=sp)
                   a5        = pi*f*t5
                   f5        = phi02*t5
                   sinc5     = f5*sin(a5)/a5
                   b5        = pi*fdif*t5
                   sinc15    = sin(b5)/b5
                   c5        = pi*fsum*t5
                   g5        = phi04*t5
                   sinc25    = sin(c5)/c5
                   sum5      = g1*(sinc5+sinc25)
                   phif(i+5) = sinc5+sum5
                   t6        = real(i+6,kind=sp)
                   a6        = pi*f*t6
                   f6        = phi02*t6
                   sinc6     = f6*sin(a6)/a6
                   b6        = pi*fdif*t1
                   sinc16    = sin(b6)/b6
                   c6        = pi*fsum*t6
                   g6        = phi04*t6
                   sinc26    = sin(c6)/c6
                   sum6      = g6*(sinc6+sinc26)
                   phif(i+6)  = sinc6+sum6
                   t7         = real(i+7,kind=sp)
                   a7         = pi*f*t7
                   f7         = phi02*t7
                   sinc7      = f7*sin(a7)/a7
                   b7         = pi*fdif*t7
                   sinc17     = sin(b7)/b7
                   c7         = pi*fsum*t7
                   g7         = phi04*t7
                   sinc27     = sin(c7)/c7
                   sum7       = g7*(sinc7+sinc27)
                   phif(i+7)  = sinc7+sum7
                   t8         = real(i+8,kind=sp)
                   a8         = pi*f*t8
                   f8         = phi02*t8
                   sinc8      = f8*sin(a8)/a8
                   b8         = pi*fdif*t8
                   sinc18     = sin(b8)/b8
                   c8         = pi*fsum*t8
                   g8         = phi04*t8
                   sinc28     = sin(c8)/c8
                   sum8       = g8*(sinc8+sinc28)
                   phif(i+8)  = sinc8+sum8
                   t9         = real(i+9,kind=sp)
                   a9         = pi*f*t9
                   f9         = phi02*t9
                   sinc9      = f9*sin(a9)/a9
                   b9         = pi*fdif*t9
                   sinc19     = sin(b9)/b9
                   c9         = pi*fsum*t9
                   g9         = phi04*t9
                   sinc29     = sin(c9)/c9
                   sum9       = g9*(sinc9+sinc29)
                   phif(i+9)  = sinc9+sum9
                   t10        = real(i+10,kind=sp)
                   a10        = pi*f*t10
                   f10        = phi02*t10
                   sinc10     = f1*sin(a10)/a10
                   b10        = pi*fdif*t10
                   sinc110    = sin(b10)/b10
                   c10        = pi*fsum*t10
                   g10        = phi04*t10
                   sinc210    = sin(c10)/c10
                   sum10      = g10*(sinc10+sinc210)
                   phif(i+10) = sinc10+sum10
                   t11        = real(i+11,kind=sp)
                   a11        = pi*f*t11
                   f11        = phi02*t11
                   sinc11     = f11*sin(a11)/a11
                   b11        = pi*fdif*t11
                   sinc111    = sin(b11)/b11
                   c11        = pi*fsum*t11
                   g11        = phi04*t11
                   sinc211    = sin(c11)/c11
                   sum11      = g11*(sinc11+sinc211)
                   phif(i+11) = sinc11+sum11
                   t12        = real(i+12,kind=sp)
                   a12        = pi*f*t12
                   f12        = phi02*t12
                   sinc12     = f12*sin(a12)/a12
                   b12        = pi*fdif*t12
                   sinc112    = sin(b12)/b12
                   c12        = pi*fsum*t12
                   g12        = phi04*t12
                   sinc212    = sin(c12)/c12
                   sum12      = g12*(sinc12+sinc212)
                   phif(i+12) = sinc12+sum12
                   t13        = real(i+13,kind=sp)
                   a13        = pi*f*t13
                   f13        = phi02*t13
                   sinc13     = f1*sin(a13)/a13
                   b13        = pi*fdif*t13
                   sinc113    = sin(b13)/b13
                   c13        = pi*fsum*t13
                   g13        = phi04*t13
                   sinc213    = sin(c13)/c13
                   sum13      = g13*(sinc13+sinc213)
                   phif(i+13) = sinc13+sum13
                   t14        = real(i+14,kind=sp)
                   a14        = pi*f*t14
                   f14        = phi02*t14
                   sinc14     = f1*sin(a14)/a14
                   b14        = pi*fdif*t14
                   sinc114    = sin(b14)/b14
                   c14        = pi*fsum*t14
                   g14        = phi04*t14
                   sinc214    = sin(c14)/c14
                   sum14      = g1*(sinc14+sinc214)
                   phif(i+14) = sinc14+sum14
                   t15        = real(i+15,kind=sp)
                   a15        = pi*f*t15
                   f15        = phi02*t15
                   sinc15     = f15*sin(a15)/a15
                   b15        = pi*fdif*t15
                   sinc115    = sin(b15)/b15
                   c15        = pi*fsum*t15
                   g15        = phi04*t15
                   sinc215    = sin(c15)/c15
                   sum15      = g15*(sinc15+sinc215)
                   phif(i+15) = sinc15+sum15 
               end do
           end if
       end subroutine raster_flux_mod_sinc_unroll_16x_r4


       subroutine raster_flux_mod_sinc_unroll_16x_r8(phif,htin,f,phi0,f0,rho0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_mod_sinc_unroll_16x_r8
           !dir$ attributes forceinline ::   raster_flux_mod_sinc_unroll_16x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_flux_mod_sinc_unroll_16x_r8
           real(kind=dp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=dp),                         intent(in)  :: f
           real(kind=dp),                         intent(in)  :: phi0
           real(kind=dp),                         intent(in)  :: f0
           real(kind=dp),                         intent(in)  :: rho0
           real(kind=dp), parameter :: pi = 3.14159265358979323846264338328_dp
           real(kind=dp), automatic :: sinc0,sinc1,sinc2,sinc3,sinc4,sinc5,sinc6,sinc7
           real(kind=dp), automatic :: sinc8,sinc9,sinc10,sinc11,sinc12,sinc13,sinc14,sinc15
           real(kind=dp), automatic :: sinc10,sinc11,sinc12,sinc13,sinc14,sinc15,sinc16,sinc17
           real(kind=dp), automatic :: sinc18,sinc19,sinc110,sinc111,sinc112,sinc113,sinc114,sinc115
           real(kind=dp), automatic :: sinc20,sinc21,sinc22,sinc23,sinc24,sinc25,sinc26,sinc27
           real(kind=dp), automatic :: sinc28,sinc29,sinc210,sinc211,sinc212,sinc213,sinc214,sinc215
           real(kind=dp), automatic :: a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15
           real(kind=dp), automatic :: b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15
           real(kind=dp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15
           real(kind=dp), automatic :: c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15
           real(kind=dp), automatic :: sum0,sum1,sum2,sum3,sum4,sum5,sum6,sum7
           real(kind=dp), automatic :: sum8,sum9,sum10,sum11,sum12,sum13,sum14,sum15
           real(kind=dp), automatic :: f0,f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15
           real(kind=dp), automatic :: g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15
           real(kind=dp), automatic :: fdif,fsum,phi02,phi04
           integer(kind=i4) :: i,m,m1,tin,nhtin
           tin = htin*2
           nhtin = -htin
           fdif  = f-f0
           fsum  = f+f0
           phi02 = 0.5_dp*phi0
           phi04 = 0.25_dp*phi0
           if(rho0/=0.5_dp) then
              call raster_flux_sinc_unroll_16x_r8(phif,htin,f,phi0)
              return
           else
              m = n(tin,16)
              if(m /= 0) then
                 do i=1,m
                    t0      = real(i,kind=dp)
                    a0      = pi*f*t0
                    f0      = phi02*t0
                    sinc0   = f0*sin(a0)/a0
                    b0      = pi*fdif*t0
                    sinc10  = sin(b0)/b0
                    c0      = pi*fsum*t0
                    g0      = phi04*t0
                    sinc20  = sin(c0)/c0
                    sum0    = g0*(sinc0+sinc20)
                   phif(i) = sinc0+sum0
                 end do
                 if(tin<16) return
              end if
              m1 = m+1
              ! Negative half first
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(8)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=nhtin,m,-16
                   t0        = real(i,kind=dp)
                   a0        = pi*f*t0
                   f0        = phi02*t0
                   sinc0     = f0*sin(a0)/a0
                   b0        = pi*fdif*t0
                   sinc10    = sin(b0)/b0
                   c0        = pi*fsum*t0
                   g0        = phi04*t0
                   sinc20    = sin(c0)/c0
                   sum0      = g0*(sinc0+sinc20)
                   phif(i)   = sinc0+sum0
                   t1        = real(i-1,kind=dp)
                   a1        = pi*f*t1
                   f1        = phi02*t1
                   sinc1     = f1*sin(a1)/a1
                   b1        = pi*fdif*t1
                   sinc11    = sin(b1)/b1
                   c1        = pi*fsum*t1
                   g1        = phi04*t1
                   sinc21    = sin(c1)/c1
                   sum1      = g1*(sinc1+sinc21)
                   phif(i-1) = sinc1+sum1
                   t2        = real(i-2,kind=dp)
                   a2        = pi*f*t2
                   f2        = phi02*t2
                   sinc2     = f2*sin(a2)/a2
                   b2        = pi*fdif*t2
                   sinc12    = sin(b2)/b2
                   c2        = pi*fsum*t2
                   g2        = phi04*t2
                   sinc22    = sin(c2)/c2
                   sum2      = g2*(sinc2+sinc22)
                   phif(i-2) = sinc2+sum2
                   t3        = real(i-3,kind=dp)
                   a3        = pi*f*t3
                   f3        = phi02*t3
                   sinc3     = f3*sin(a3)/a3
                   b3        = pi*fdif*t3
                   sinc13    = sin(b3)/b3
                   c3        = pi*fsum*t3
                   g3        = phi04*t3
                   sinc23    = sin(c3)/c3
                   sum3      = g3*(sinc3+sinc23)
                   phif(i-3) = sinc3+sum3
                   t4        = real(i-4,kind=dp)
                   a4        = pi*f*t4
                   f4        = phi02*t4
                   sinc4     = f4*sin(a4)/a4
                   b4        = pi*fdif*t4
                   sinc11    = sin(b1)/b1
                   c4        = pi*fsum*t4
                   g4        = phi04*t4
                   sinc24    = sin(c4)/c4
                   sum4      = g4*(sinc4+sinc24)
                   phif(i-4) = sinc4+sum4
                   t5        = real(i-5,kind=dp)
                   a5        = pi*f*t5
                   f5        = phi02*t5
                   sinc5     = f5*sin(a5)/a5
                   b5        = pi*fdif*t5
                   sinc15    = sin(b5)/b5
                   c5        = pi*fsum*t5
                   g5        = phi04*t5
                   sinc25    = sin(c5)/c5
                   sum5      = g1*(sinc5+sinc25)
                   phif(i-5) = sinc5+sum5
                   t6        = real(i-6,kind=dp)
                   a6        = pi*f*t6
                   f6        = phi02*t6
                   sinc6     = f6*sin(a6)/a6
                   b6        = pi*fdif*t1
                   sinc16    = sin(b6)/b6
                   c6        = pi*fsum*t6
                   g6        = phi04*t6
                   sinc26    = sin(c6)/c6
                   sum6      = g6*(sinc6+sinc26)
                   phif(i-6) = sinc6+sum6
                   t7        = real(i-7,kind=dp)
                   a7        = pi*f*t7
                   f7        = phi02*t7
                   sinc7     = f7*sin(a7)/a7
                   b7        = pi*fdif*t7
                   sinc17    = sin(b7)/b7
                   c7        = pi*fsum*t7
                   g7        = phi04*t7
                   sinc27    = sin(c7)/c7
                   sum7      = g7*(sinc7+sinc27)
                   phif(i-7) = sinc7+sum7
                   t8        = real(i-8,kind=dp)
                   a8        = pi*f*t8
                   f8        = phi02*t8
                   sinc8      = f8*sin(a8)/a8
                   b8         = pi*fdif*t8
                   sinc18     = sin(b8)/b8
                   c8         = pi*fsum*t8
                   g8         = phi04*t8
                   sinc28     = sin(c8)/c8
                   sum8       = g8*(sinc8+sinc28)
                   phif(i-8)  = sinc8+sum8
                   t9         = real(i-9,kind=dp)
                   a9         = pi*f*t9
                   f9         = phi02*t9
                   sinc9      = f9*sin(a9)/a9
                   b9         = pi*fdif*t9
                   sinc19     = sin(b9)/b9
                   c9         = pi*fsum*t9
                   g9         = phi04*t9
                   sinc29     = sin(c9)/c9
                   sum9       = g9*(sinc9+sinc29)
                   phif(i-9)  = sinc9+sum9
                   t10        = real(i-10,kind=dp)
                   a10        = pi*f*t10
                   f10        = phi02*t10
                   sinc10     = f1*sin(a10)/a10
                   b10        = pi*fdif*t10
                   sinc110    = sin(b10)/b10
                   c10        = pi*fsum*t10
                   g10        = phi04*t10
                   sinc210    = sin(c10)/c10
                   sum10      = g10*(sinc10+sinc210)
                   phif(i-10) = sinc10+sum10
                   t11        = real(i-11,kind=dp)
                   a11        = pi*f*t11
                   f11        = phi02*t11
                   sinc11     = f11*sin(a11)/a11
                   b11        = pi*fdif*t11
                   sinc111    = sin(b11)/b11
                   c11        = pi*fsum*t11
                   g11        = phi04*t11
                   sinc211    = sin(c11)/c11
                   sum11      = g11*(sinc11+sinc211)
                   phif(i-11) = sinc11+sum11
                   t12        = real(i-12,kind=dp)
                   a12        = pi*f*t12
                   f12        = phi02*t12
                   sinc12     = f12*sin(a12)/a12
                   b12        = pi*fdif*t12
                   sinc112    = sin(b12)/b12
                   c12        = pi*fsum*t12
                   g12        = phi04*t12
                   sinc212    = sin(c12)/c12
                   sum12      = g12*(sinc12+sinc212)
                   phif(i-12) = sinc12+sum12
                   t13        = real(i-13,kind=dp)
                   a13        = pi*f*t13
                   f13        = phi02*t13
                   sinc13     = f1*sin(a13)/a13
                   b13        = pi*fdif*t13
                   sinc113    = sin(b13)/b13
                   c13        = pi*fsum*t13
                   g13        = phi04*t13
                   sinc213    = sin(c13)/c13
                   sum13      = g13*(sinc13+sinc213)
                   phif(i-13) = sinc13+sum13
                   t14        = real(i-14,kind=dp)
                   a14        = pi*f*t14
                   f14        = phi02*t14
                   sinc14     = f1*sin(a14)/a14
                   b14        = pi*fdif*t14
                   sinc114    = sin(b14)/b14
                   c14        = pi*fsum*t14
                   g14        = phi04*t14
                   sinc214    = sin(c14)/c14
                   sum14      = g1*(sinc14+sinc214)
                   phif(i-14) = sinc14+sum14
                   t15        = real(i-15,kind=dp)
                   a15        = pi*f*t15
                   f15        = phi02*t15
                   sinc15     = f15*sin(a15)/a15
                   b15        = pi*fdif*t15
                   sinc115    = sin(b15)/b15
                   c15        = pi*fsum*t15
                   g15        = phi04*t15
                   sinc215    = sin(c15)/c15
                   sum15      = g15*(sinc15+sinc215)
                   phif(i-15) = sinc15+sum15 
               end do
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(8)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=m,htin,16
                   t0        = real(i,kind=dp)
                   a0        = pi*f*t0
                   f0        = phi02*t0
                   sinc0     = f0*sin(a0)/a0
                   b0        = pi*fdif*t0
                   sinc10    = sin(b0)/b0
                   c0        = pi*fsum*t0
                   g0        = phi04*t0
                   sinc20    = sin(c0)/c0
                   sum0      = g0*(sinc0+sinc20)
                   phif(i)   = sinc0+sum0
                   t1        = real(i+1,kind=dp)
                   a1        = pi*f*t1
                   f1        = phi02*t1
                   sinc1     = f1*sin(a1)/a1
                   b1        = pi*fdif*t1
                   sinc11    = sin(b1)/b1
                   c1        = pi*fsum*t1
                   g1        = phi04*t1
                   sinc21    = sin(c1)/c1
                   sum1      = g1*(sinc1+sinc21)
                   phif(i+1) = sinc1+sum1
                   t2        = real(i+2,kind=dp)
                   a2        = pi*f*t2
                   f2        = phi02*t2
                   sinc2     = f2*sin(a2)/a2
                   b2        = pi*fdif*t2
                   sinc12    = sin(b2)/b2
                   c2        = pi*fsum*t2
                   g2        = phi04*t2
                   sinc22    = sin(c2)/c2
                   sum2      = g2*(sinc2+sinc22)
                   phif(i+2) = sinc2+sum2
                   t3        = real(i+3,kind=dp)
                   a3        = pi*f*t3
                   f3        = phi02*t3
                   sinc3     = f3*sin(a3)/a3
                   b3        = pi*fdif*t3
                   sinc13    = sin(b3)/b3
                   c3        = pi*fsum*t3
                   g3        = phi04*t3
                   sinc23    = sin(c3)/c3
                   sum3      = g3*(sinc3+sinc23)
                   phif(i+3) = sinc3+sum3
                   t4        = real(i+4,kind=dp)
                   a4        = pi*f*t4
                   f4        = phi02*t4
                   sinc4     = f4*sin(a4)/a4
                   b4        = pi*fdif*t4
                   sinc11    = sin(b1)/b1
                   c4        = pi*fsum*t4
                   g4        = phi04*t4
                   sinc24    = sin(c4)/c4
                   sum4      = g4*(sinc4+sinc24)
                   phif(i+4) = sinc4+sum4
                   t5        = real(i+5,kind=dp)
                   a5        = pi*f*t5
                   f5        = phi02*t5
                   sinc5     = f5*sin(a5)/a5
                   b5        = pi*fdif*t5
                   sinc15    = sin(b5)/b5
                   c5        = pi*fsum*t5
                   g5        = phi04*t5
                   sinc25    = sin(c5)/c5
                   sum5      = g1*(sinc5+sinc25)
                   phif(i+5) = sinc5+sum5
                   t6        = real(i+6,kind=dp)
                   a6        = pi*f*t6
                   f6        = phi02*t6
                   sinc6     = f6*sin(a6)/a6
                   b6        = pi*fdif*t1
                   sinc16    = sin(b6)/b6
                   c6        = pi*fsum*t6
                   g6        = phi04*t6
                   sinc26    = sin(c6)/c6
                   sum6      = g6*(sinc6+sinc26)
                   phif(i+6)  = sinc6+sum6
                   t7         = real(i+7,kind=dp)
                   a7         = pi*f*t7
                   f7         = phi02*t7
                   sinc7      = f7*sin(a7)/a7
                   b7         = pi*fdif*t7
                   sinc17     = sin(b7)/b7
                   c7         = pi*fsum*t7
                   g7         = phi04*t7
                   sinc27     = sin(c7)/c7
                   sum7       = g7*(sinc7+sinc27)
                   phif(i+7)  = sinc7+sum7
                   t8         = real(i+8,kind=dp)
                   a8         = pi*f*t8
                   f8         = phi02*t8
                   sinc8      = f8*sin(a8)/a8
                   b8         = pi*fdif*t8
                   sinc18     = sin(b8)/b8
                   c8         = pi*fsum*t8
                   g8         = phi04*t8
                   sinc28     = sin(c8)/c8
                   sum8       = g8*(sinc8+sinc28)
                   phif(i+8)  = sinc8+sum8
                   t9         = real(i+9,kind=dp)
                   a9         = pi*f*t9
                   f9         = phi02*t9
                   sinc9      = f9*sin(a9)/a9
                   b9         = pi*fdif*t9
                   sinc19     = sin(b9)/b9
                   c9         = pi*fsum*t9
                   g9         = phi04*t9
                   sinc29     = sin(c9)/c9
                   sum9       = g9*(sinc9+sinc29)
                   phif(i+9)  = sinc9+sum9
                   t10        = real(i+10,kind=dp)
                   a10        = pi*f*t10
                   f10        = phi02*t10
                   sinc10     = f1*sin(a10)/a10
                   b10        = pi*fdif*t10
                   sinc110    = sin(b10)/b10
                   c10        = pi*fsum*t10
                   g10        = phi04*t10
                   sinc210    = sin(c10)/c10
                   sum10      = g10*(sinc10+sinc210)
                   phif(i+10) = sinc10+sum10
                   t11        = real(i+11,kind=dp)
                   a11        = pi*f*t11
                   f11        = phi02*t11
                   sinc11     = f11*sin(a11)/a11
                   b11        = pi*fdif*t11
                   sinc111    = sin(b11)/b11
                   c11        = pi*fsum*t11
                   g11        = phi04*t11
                   sinc211    = sin(c11)/c11
                   sum11      = g11*(sinc11+sinc211)
                   phif(i+11) = sinc11+sum11
                   t12        = real(i+12,kind=dp)
                   a12        = pi*f*t12
                   f12        = phi02*t12
                   sinc12     = f12*sin(a12)/a12
                   b12        = pi*fdif*t12
                   sinc112    = sin(b12)/b12
                   c12        = pi*fsum*t12
                   g12        = phi04*t12
                   sinc212    = sin(c12)/c12
                   sum12      = g12*(sinc12+sinc212)
                   phif(i+12) = sinc12+sum12
                   t13        = real(i+13,kind=dp)
                   a13        = pi*f*t13
                   f13        = phi02*t13
                   sinc13     = f1*sin(a13)/a13
                   b13        = pi*fdif*t13
                   sinc113    = sin(b13)/b13
                   c13        = pi*fsum*t13
                   g13        = phi04*t13
                   sinc213    = sin(c13)/c13
                   sum13      = g13*(sinc13+sinc213)
                   phif(i+13) = sinc13+sum13
                   t14        = real(i+14,kind=dp)
                   a14        = pi*f*t14
                   f14        = phi02*t14
                   sinc14     = f1*sin(a14)/a14
                   b14        = pi*fdif*t14
                   sinc114    = sin(b14)/b14
                   c14        = pi*fsum*t14
                   g14        = phi04*t14
                   sinc214    = sin(c14)/c14
                   sum14      = g1*(sinc14+sinc214)
                   phif(i+14) = sinc14+sum14
                   t15        = real(i+15,kind=dp)
                   a15        = pi*f*t15
                   f15        = phi02*t15
                   sinc15     = f15*sin(a15)/a15
                   b15        = pi*fdif*t15
                   sinc115    = sin(b15)/b15
                   c15        = pi*fsum*t15
                   g15        = phi04*t15
                   sinc215    = sin(c15)/c15
                   sum15      = g15*(sinc15+sinc215)
                   phif(i+15) = sinc15+sum15 
               end do
           end if
       end subroutine raster_flux_mod_sinc_unroll_16x_r8


       subroutine raster_flux_mod_sinc_unroll_8x_r4(phif,htin,f,phi0,f0,rho0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_mod_sinc_unroll_8x_r4
           !dir$ attributes forceinline ::   raster_flux_mod_sinc_unroll_8x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_flux_mod_sinc_unroll_8x_r4
           real(kind=sp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=sp),                         intent(in)  :: f
           real(kind=sp),                         intent(in)  :: phi0
           real(kind=sp),                         intent(in)  :: f0
           real(kind=sp),                         intent(in)  :: rho0
           real(kind=sp), parameter :: pi = 3.14159265358979323846264338328_sp
           real(kind=sp), automatic :: sinc0,sinc1,sinc2,sinc3,sinc4,sinc5,sinc6,sinc7
           real(kind=sp), automatic :: sinc10,sinc11,sinc12,sinc13,sinc14,sinc15,sinc16,sinc17
           real(kind=sp), automatic :: sinc20,sinc21,sinc22,sinc23,sinc24,sinc25,sinc26,sinc27
           real(kind=sp), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
           real(kind=sp), automatic :: b0,b1,b2,b3,b4,b5,b6,b7
           real(kind=sp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=sp), automatic :: c0,c1,c2,c3,c4,c5,c6,c7
           real(kind=sp), automatic :: sum0,sum1,sum2,sum3,sum4,sum5,sum6,sum7
           real(kind=sp), automatic :: f0,f1,f2,f3,f4,f5,f6,f7
           real(kind=sp), automatic :: g0,g1,g2,g3,g4,g5,g6,g7
           real(kind=sp), automatic :: fdif,fsum,phi02,phi04
           integer(kind=i4) :: i,m,m1,tin,nhtin
           tin = htin*2
           nhtin = -htin
           fdif  = f-f0
           fsum  = f+f0
           phi02 = 0.5_sp*phi0
           phi04 = 0.25_sp*phi0
           if(rho0/=0.5_sp) then
              call raster_flux_sinc_unroll_8x_r4(phif,htin,f,phi0)
              return
           else
              m = n(tin,8)
              if(m /= 0) then
                 do i=1,m
                    t0      = real(i,kind=sp)
                    a0      = pi*f*t0
                    f0      = phi02*t0
                    sinc0   = f0*sin(a0)/a0
                    b0      = pi*fdif*t0
                    sinc10  = sin(b0)/b0
                    c0      = pi*fsum*t0
                    g0      = phi04*t0
                    sinc20  = sin(c0)/c0
                    sum0    = g0*(sinc0+sinc20)
                   phif(i) = sinc0+sum0
                 end do
                 if(tin<8) return
              end if
              m1 = m+1
              ! Negative half first
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(4)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=nhtin,m,-8
                   t0        = real(i,kind=sp)
                   a0        = pi*f*t0
                   f0        = phi02*t0
                   sinc0     = f0*sin(a0)/a0
                   b0        = pi*fdif*t0
                   sinc10    = sin(b0)/b0
                   c0        = pi*fsum*t0
                   g0        = phi04*t0
                   sinc20    = sin(c0)/c0
                   sum0      = g0*(sinc0+sinc20)
                   phif(i)   = sinc0+sum0
                   t1        = real(i-1,kind=sp)
                   a1        = pi*f*t1
                   f1        = phi02*t1
                   sinc1     = f1*sin(a1)/a1
                   b1        = pi*fdif*t1
                   sinc11    = sin(b1)/b1
                   c1        = pi*fsum*t1
                   g1        = phi04*t1
                   sinc21    = sin(c1)/c1
                   sum1      = g1*(sinc1+sinc21)
                   phif(i-1) = sinc1+sum1
                   t2        = real(i-2,kind=sp)
                   a2        = pi*f*t2
                   f2        = phi02*t2
                   sinc2     = f2*sin(a2)/a2
                   b2        = pi*fdif*t2
                   sinc12    = sin(b2)/b2
                   c2        = pi*fsum*t2
                   g2        = phi04*t2
                   sinc22    = sin(c2)/c2
                   sum2      = g2*(sinc2+sinc22)
                   phif(i-2) = sinc2+sum2
                   t3        = real(i-3,kind=sp)
                   a3        = pi*f*t3
                   f3        = phi02*t3
                   sinc3     = f3*sin(a3)/a3
                   b3        = pi*fdif*t3
                   sinc13    = sin(b3)/b3
                   c3        = pi*fsum*t3
                   g3        = phi04*t3
                   sinc23    = sin(c3)/c3
                   sum3      = g3*(sinc3+sinc23)
                   phif(i-3) = sinc3+sum3
                   t4        = real(i-4,kind=sp)
                   a4        = pi*f*t4
                   f4        = phi02*t4
                   sinc4     = f4*sin(a4)/a4
                   b4        = pi*fdif*t4
                   sinc11    = sin(b1)/b1
                   c4        = pi*fsum*t4
                   g4        = phi04*t4
                   sinc24    = sin(c4)/c4
                   sum4      = g4*(sinc4+sinc24)
                   phif(i-4) = sinc4+sum4
                   t5        = real(i-5,kind=sp)
                   a5        = pi*f*t5
                   f5        = phi02*t5
                   sinc5     = f5*sin(a5)/a5
                   b5        = pi*fdif*t5
                   sinc15    = sin(b5)/b5
                   c5        = pi*fsum*t5
                   g5        = phi04*t5
                   sinc25    = sin(c5)/c5
                   sum5      = g1*(sinc5+sinc25)
                   phif(i-5) = sinc5+sum5
                   t6        = real(i-6,kind=sp)
                   a6        = pi*f*t6
                   f6        = phi02*t6
                   sinc6     = f6*sin(a6)/a6
                   b6        = pi*fdif*t1
                   sinc16    = sin(b6)/b6
                   c6        = pi*fsum*t6
                   g6        = phi04*t6
                   sinc26    = sin(c6)/c6
                   sum6      = g6*(sinc6+sinc26)
                   phif(i-6) = sinc6+sum6
                   t7        = real(i-7,kind=sp)
                   a7        = pi*f*t7
                   f7        = phi02*t7
                   sinc7     = f7*sin(a7)/a7
                   b7        = pi*fdif*t7
                   sinc17    = sin(b7)/b7
                   c7        = pi*fsum*t7
                   g7        = phi04*t7
                   sinc27    = sin(c7)/c7
                   sum7      = g7*(sinc7+sinc27)
                   phif(i-7) = sinc7+sum7
              end do
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(4)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=m,htin,8
                   t0        = real(i,kind=sp)
                   a0        = pi*f*t0
                   f0        = phi02*t0
                   sinc0     = f0*sin(a0)/a0
                   b0        = pi*fdif*t0
                   sinc10    = sin(b0)/b0
                   c0        = pi*fsum*t0
                   g0        = phi04*t0
                   sinc20    = sin(c0)/c0
                   sum0      = g0*(sinc0+sinc20)
                   phif(i)   = sinc0+sum0
                   t1        = real(i+1,kind=sp)
                   a1        = pi*f*t1
                   f1        = phi02*t1
                   sinc1     = f1*sin(a1)/a1
                   b1        = pi*fdif*t1
                   sinc11    = sin(b1)/b1
                   c1        = pi*fsum*t1
                   g1        = phi04*t1
                   sinc21    = sin(c1)/c1
                   sum1      = g1*(sinc1+sinc21)
                   phif(i+1) = sinc1+sum1
                   t2        = real(i+2,kind=sp)
                   a2        = pi*f*t2
                   f2        = phi02*t2
                   sinc2     = f2*sin(a2)/a2
                   b2        = pi*fdif*t2
                   sinc12    = sin(b2)/b2
                   c2        = pi*fsum*t2
                   g2        = phi04*t2
                   sinc22    = sin(c2)/c2
                   sum2      = g2*(sinc2+sinc22)
                   phif(i+2) = sinc2+sum2
                   t3        = real(i+3,kind=sp)
                   a3        = pi*f*t3
                   f3        = phi02*t3
                   sinc3     = f3*sin(a3)/a3
                   b3        = pi*fdif*t3
                   sinc13    = sin(b3)/b3
                   c3        = pi*fsum*t3
                   g3        = phi04*t3
                   sinc23    = sin(c3)/c3
                   sum3      = g3*(sinc3+sinc23)
                   phif(i+3) = sinc3+sum3
                   t4        = real(i+4,kind=sp)
                   a4        = pi*f*t4
                   f4        = phi02*t4
                   sinc4     = f4*sin(a4)/a4
                   b4        = pi*fdif*t4
                   sinc11    = sin(b1)/b1
                   c4        = pi*fsum*t4
                   g4        = phi04*t4
                   sinc24    = sin(c4)/c4
                   sum4      = g4*(sinc4+sinc24)
                   phif(i+4) = sinc4+sum4
                   t5        = real(i+5,kind=sp)
                   a5        = pi*f*t5
                   f5        = phi02*t5
                   sinc5     = f5*sin(a5)/a5
                   b5        = pi*fdif*t5
                   sinc15    = sin(b5)/b5
                   c5        = pi*fsum*t5
                   g5        = phi04*t5
                   sinc25    = sin(c5)/c5
                   sum5      = g1*(sinc5+sinc25)
                   phif(i+5) = sinc5+sum5
                   t6        = real(i+6,kind=sp)
                   a6        = pi*f*t6
                   f6        = phi02*t6
                   sinc6     = f6*sin(a6)/a6
                   b6        = pi*fdif*t1
                   sinc16    = sin(b6)/b6
                   c6        = pi*fsum*t6
                   g6        = phi04*t6
                   sinc26    = sin(c6)/c6
                   sum6      = g6*(sinc6+sinc26)
                   phif(i+6)  = sinc6+sum6
                   t7         = real(i+7,kind=sp)
                   a7         = pi*f*t7
                   f7         = phi02*t7
                   sinc7      = f7*sin(a7)/a7
                   b7         = pi*fdif*t7
                   sinc17     = sin(b7)/b7
                   c7         = pi*fsum*t7
                   g7         = phi04*t7
                   sinc27     = sin(c7)/c7
                   sum7       = g7*(sinc7+sinc27)
                   phif(i+7)  = sinc7+sum7
                  
               end do
           end if
       end subroutine raster_flux_mod_sinc_unroll_8x_r4

                                  
       subroutine raster_flux_mod_sinc_unroll_8x_r8(phif,htin,f,phi0,f0,rho0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_mod_sinc_unroll_8x_r8
           !dir$ attributes forceinline ::   raster_flux_mod_sinc_unroll_8x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_flux_mod_sinc_unroll_8x_r8
           real(kind=dp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=dp),                         intent(in)  :: f
           real(kind=dp),                         intent(in)  :: phi0
           real(kind=dp),                         intent(in)  :: f0
           real(kind=dp),                         intent(in)  :: rho0
           real(kind=dp), parameter :: pi = 3.14159265358979323846264338328_dp
           real(kind=dp), automatic :: sinc0,sinc1,sinc2,sinc3,sinc4,sinc5,sinc6,sinc7
           real(kind=dp), automatic :: sinc10,sinc11,sinc12,sinc13,sinc14,sinc15,sinc16,sinc17
           real(kind=dp), automatic :: sinc20,sinc21,sinc22,sinc23,sinc24,sinc25,sinc26,sinc27
           real(kind=dp), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
           real(kind=dp), automatic :: b0,b1,b2,b3,b4,b5,b6,b7
           real(kind=dp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=dp), automatic :: c0,c1,c2,c3,c4,c5,c6,c7
           real(kind=dp), automatic :: sum0,sum1,sum2,sum3,sum4,sum5,sum6,sum7
           real(kind=dp), automatic :: f0,f1,f2,f3,f4,f5,f6,f7
           real(kind=dp), automatic :: g0,g1,g2,g3,g4,g5,g6,g7
           real(kind=dp), automatic :: fdif,fsum,phi02,phi04
           integer(kind=i4) :: i,m,m1,tin,nhtin
           tin = htin*2
           nhtin = -htin
           fdif  = f-f0
           fsum  = f+f0
           phi02 = 0.5_dp*phi0
           phi04 = 0.25_dp*phi0
           if(rho0/=0.5_dp) then
              call raster_flux_sinc_unroll_8x_r8(phif,htin,f,phi0)
              return
           else
              m = n(tin,8)
              if(m /= 0) then
                 do i=1,m
                    t0      = real(i,kind=dp)
                    a0      = pi*f*t0
                    f0      = phi02*t0
                    sinc0   = f0*sin(a0)/a0
                    b0      = pi*fdif*t0
                    sinc10  = sin(b0)/b0
                    c0      = pi*fsum*t0
                    g0      = phi04*t0
                    sinc20  = sin(c0)/c0
                    sum0    = g0*(sinc0+sinc20)
                   phif(i) = sinc0+sum0
                 end do
                 if(tin<8) return
              end if
              m1 = m+1
              ! Negative half first
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(8)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=nhtin,m,-8
                   t0        = real(i,kind=dp)
                   a0        = pi*f*t0
                   f0        = phi02*t0
                   sinc0     = f0*sin(a0)/a0
                   b0        = pi*fdif*t0
                   sinc10    = sin(b0)/b0
                   c0        = pi*fsum*t0
                   g0        = phi04*t0
                   sinc20    = sin(c0)/c0
                   sum0      = g0*(sinc0+sinc20)
                   phif(i)   = sinc0+sum0
                   t1        = real(i-1,kind=dp)
                   a1        = pi*f*t1
                   f1        = phi02*t1
                   sinc1     = f1*sin(a1)/a1
                   b1        = pi*fdif*t1
                   sinc11    = sin(b1)/b1
                   c1        = pi*fsum*t1
                   g1        = phi04*t1
                   sinc21    = sin(c1)/c1
                   sum1      = g1*(sinc1+sinc21)
                   phif(i-1) = sinc1+sum1
                   t2        = real(i-2,kind=dp)
                   a2        = pi*f*t2
                   f2        = phi02*t2
                   sinc2     = f2*sin(a2)/a2
                   b2        = pi*fdif*t2
                   sinc12    = sin(b2)/b2
                   c2        = pi*fsum*t2
                   g2        = phi04*t2
                   sinc22    = sin(c2)/c2
                   sum2      = g2*(sinc2+sinc22)
                   phif(i-2) = sinc2+sum2
                   t3        = real(i-3,kind=dp)
                   a3        = pi*f*t3
                   f3        = phi02*t3
                   sinc3     = f3*sin(a3)/a3
                   b3        = pi*fdif*t3
                   sinc13    = sin(b3)/b3
                   c3        = pi*fsum*t3
                   g3        = phi04*t3
                   sinc23    = sin(c3)/c3
                   sum3      = g3*(sinc3+sinc23)
                   phif(i-3) = sinc3+sum3
                   t4        = real(i-4,kind=dp)
                   a4        = pi*f*t4
                   f4        = phi02*t4
                   sinc4     = f4*sin(a4)/a4
                   b4        = pi*fdif*t4
                   sinc11    = sin(b1)/b1
                   c4        = pi*fsum*t4
                   g4        = phi04*t4
                   sinc24    = sin(c4)/c4
                   sum4      = g4*(sinc4+sinc24)
                   phif(i-4) = sinc4+sum4
                   t5        = real(i-5,kind=dp)
                   a5        = pi*f*t5
                   f5        = phi02*t5
                   sinc5     = f5*sin(a5)/a5
                   b5        = pi*fdif*t5
                   sinc15    = sin(b5)/b5
                   c5        = pi*fsum*t5
                   g5        = phi04*t5
                   sinc25    = sin(c5)/c5
                   sum5      = g1*(sinc5+sinc25)
                   phif(i-5) = sinc5+sum5
                   t6        = real(i-6,kind=dp)
                   a6        = pi*f*t6
                   f6        = phi02*t6
                   sinc6     = f6*sin(a6)/a6
                   b6        = pi*fdif*t1
                   sinc16    = sin(b6)/b6
                   c6        = pi*fsum*t6
                   g6        = phi04*t6
                   sinc26    = sin(c6)/c6
                   sum6      = g6*(sinc6+sinc26)
                   phif(i-6) = sinc6+sum6
                   t7        = real(i-7,kind=dp)
                   a7        = pi*f*t7
                   f7        = phi02*t7
                   sinc7     = f7*sin(a7)/a7
                   b7        = pi*fdif*t7
                   sinc17    = sin(b7)/b7
                   c7        = pi*fsum*t7
                   g7        = phi04*t7
                   sinc27    = sin(c7)/c7
                   sum7      = g7*(sinc7+sinc27)
                   phif(i-7) = sinc7+sum7
              end do
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(8)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=m,htin,8
                   t0        = real(i,kind=dp)
                   a0        = pi*f*t0
                   f0        = phi02*t0
                   sinc0     = f0*sin(a0)/a0
                   b0        = pi*fdif*t0
                   sinc10    = sin(b0)/b0
                   c0        = pi*fsum*t0
                   g0        = phi04*t0
                   sinc20    = sin(c0)/c0
                   sum0      = g0*(sinc0+sinc20)
                   phif(i)   = sinc0+sum0
                   t1        = real(i+1,kind=dp)
                   a1        = pi*f*t1
                   f1        = phi02*t1
                   sinc1     = f1*sin(a1)/a1
                   b1        = pi*fdif*t1
                   sinc11    = sin(b1)/b1
                   c1        = pi*fsum*t1
                   g1        = phi04*t1
                   sinc21    = sin(c1)/c1
                   sum1      = g1*(sinc1+sinc21)
                   phif(i+1) = sinc1+sum1
                   t2        = real(i+2,kind=dp)
                   a2        = pi*f*t2
                   f2        = phi02*t2
                   sinc2     = f2*sin(a2)/a2
                   b2        = pi*fdif*t2
                   sinc12    = sin(b2)/b2
                   c2        = pi*fsum*t2
                   g2        = phi04*t2
                   sinc22    = sin(c2)/c2
                   sum2      = g2*(sinc2+sinc22)
                   phif(i+2) = sinc2+sum2
                   t3        = real(i+3,kind=dp)
                   a3        = pi*f*t3
                   f3        = phi02*t3
                   sinc3     = f3*sin(a3)/a3
                   b3        = pi*fdif*t3
                   sinc13    = sin(b3)/b3
                   c3        = pi*fsum*t3
                   g3        = phi04*t3
                   sinc23    = sin(c3)/c3
                   sum3      = g3*(sinc3+sinc23)
                   phif(i+3) = sinc3+sum3
                   t4        = real(i+4,kind=dp)
                   a4        = pi*f*t4
                   f4        = phi02*t4
                   sinc4     = f4*sin(a4)/a4
                   b4        = pi*fdif*t4
                   sinc11    = sin(b1)/b1
                   c4        = pi*fsum*t4
                   g4        = phi04*t4
                   sinc24    = sin(c4)/c4
                   sum4      = g4*(sinc4+sinc24)
                   phif(i+4) = sinc4+sum4
                   t5        = real(i+5,kind=dp)
                   a5        = pi*f*t5
                   f5        = phi02*t5
                   sinc5     = f5*sin(a5)/a5
                   b5        = pi*fdif*t5
                   sinc15    = sin(b5)/b5
                   c5        = pi*fsum*t5
                   g5        = phi04*t5
                   sinc25    = sin(c5)/c5
                   sum5      = g1*(sinc5+sinc25)
                   phif(i+5) = sinc5+sum5
                   t6        = real(i+6,kind=dp)
                   a6        = pi*f*t6
                   f6        = phi02*t6
                   sinc6     = f6*sin(a6)/a6
                   b6        = pi*fdif*t1
                   sinc16    = sin(b6)/b6
                   c6        = pi*fsum*t6
                   g6        = phi04*t6
                   sinc26    = sin(c6)/c6
                   sum6      = g6*(sinc6+sinc26)
                   phif(i+6)  = sinc6+sum6
                   t7         = real(i+7,kind=dp)
                   a7         = pi*f*t7
                   f7         = phi02*t7
                   sinc7      = f7*sin(a7)/a7
                   b7         = pi*fdif*t7
                   sinc17     = sin(b7)/b7
                   c7         = pi*fsum*t7
                   g7         = phi04*t7
                   sinc27     = sin(c7)/c7
                   sum7       = g7*(sinc7+sinc27)
                   phif(i+7)  = sinc7+sum7
                
               end do
           end if
       end subroutine raster_flux_mod_sinc_unroll_8x_r8


       subroutine raster_flux_mod_sinc_unroll_4x_r4(phif,htin,f,phi0,f0,rho0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_mod_sinc_unroll_4x_r4
           !dir$ attributes forceinline ::   raster_flux_mod_sinc_unroll_4x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_flux_mod_sinc_unroll_4x_r4
           real(kind=sp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=sp),                         intent(in)  :: f
           real(kind=sp),                         intent(in)  :: phi0
           real(kind=sp),                         intent(in)  :: f0
           real(kind=sp),                         intent(in)  :: rho0
           real(kind=sp), parameter :: pi = 3.14159265358979323846264338328_sp
           real(kind=sp), automatic :: sinc0,sinc1,sinc2,sinc3
           real(kind=sp), automatic :: sinc10,sinc11,sinc12,sinc13
           real(kind=sp), automatic :: sinc20,sinc21,sinc22,sinc23
           real(kind=sp), automatic :: a0,a1,a2,a3
           real(kind=sp), automatic :: b0,b1,b2,b3
           real(kind=sp), automatic :: t0,t1,t2,t3
           real(kind=sp), automatic :: c0,c1,c2,c3
           real(kind=sp), automatic :: sum0,sum1,sum2,sum3
           real(kind=sp), automatic :: f0,f1,f2,f3
           real(kind=sp), automatic :: g0,g1,g2,g3
           real(kind=sp), automatic :: fdif,fsum,phi02,phi04
           integer(kind=i4) :: i,m,m1,tin,nhtin
           tin = htin*2
           nhtin = -htin
           fdif  = f-f0
           fsum  = f+f0
           phi02 = 0.5_sp*phi0
           phi04 = 0.25_sp*phi0
           if(rho0/=0.5_sp) then
              call raster_flux_sinc_unroll_4x_r4(phif,htin,f,phi0)
              return
           else
              m = n(tin,4)
              if(m /= 0) then
                 do i=1,m
                    t0      = real(i,kind=sp)
                    a0      = pi*f*t0
                    f0      = phi02*t0
                    sinc0   = f0*sin(a0)/a0
                    b0      = pi*fdif*t0
                    sinc10  = sin(b0)/b0
                    c0      = pi*fsum*t0
                    g0      = phi04*t0
                    sinc20  = sin(c0)/c0
                    sum0    = g0*(sinc0+sinc20)
                   phif(i) = sinc0+sum0
                 end do
                 if(tin<4) return
              end if
              m1 = m+1
              ! Negative half first
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(4)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=nhtin,m,-4
                   t0        = real(i,kind=sp)
                   a0        = pi*f*t0
                   f0        = phi02*t0
                   sinc0     = f0*sin(a0)/a0
                   b0        = pi*fdif*t0
                   sinc10    = sin(b0)/b0
                   c0        = pi*fsum*t0
                   g0        = phi04*t0
                   sinc20    = sin(c0)/c0
                   sum0      = g0*(sinc0+sinc20)
                   phif(i)   = sinc0+sum0
                   t1        = real(i-1,kind=sp)
                   a1        = pi*f*t1
                   f1        = phi02*t1
                   sinc1     = f1*sin(a1)/a1
                   b1        = pi*fdif*t1
                   sinc11    = sin(b1)/b1
                   c1        = pi*fsum*t1
                   g1        = phi04*t1
                   sinc21    = sin(c1)/c1
                   sum1      = g1*(sinc1+sinc21)
                   phif(i-1) = sinc1+sum1
                   t2        = real(i-2,kind=sp)
                   a2        = pi*f*t2
                   f2        = phi02*t2
                   sinc2     = f2*sin(a2)/a2
                   b2        = pi*fdif*t2
                   sinc12    = sin(b2)/b2
                   c2        = pi*fsum*t2
                   g2        = phi04*t2
                   sinc22    = sin(c2)/c2
                   sum2      = g2*(sinc2+sinc22)
                   phif(i-2) = sinc2+sum2
                   t3        = real(i-3,kind=sp)
                   a3        = pi*f*t3
                   f3        = phi02*t3
                   sinc3     = f3*sin(a3)/a3
                   b3        = pi*fdif*t3
                   sinc13    = sin(b3)/b3
                   c3        = pi*fsum*t3
                   g3        = phi04*t3
                   sinc23    = sin(c3)/c3
                   sum3      = g3*(sinc3+sinc23)
                   phif(i-3) = sinc3+sum3
              end do
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(4)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=m,htin,4
                   t0        = real(i,kind=sp)
                   a0        = pi*f*t0
                   f0        = phi02*t0
                   sinc0     = f0*sin(a0)/a0
                   b0        = pi*fdif*t0
                   sinc10    = sin(b0)/b0
                   c0        = pi*fsum*t0
                   g0        = phi04*t0
                   sinc20    = sin(c0)/c0
                   sum0      = g0*(sinc0+sinc20)
                   phif(i)   = sinc0+sum0
                   t1        = real(i+1,kind=sp)
                   a1        = pi*f*t1
                   f1        = phi02*t1
                   sinc1     = f1*sin(a1)/a1
                   b1        = pi*fdif*t1
                   sinc11    = sin(b1)/b1
                   c1        = pi*fsum*t1
                   g1        = phi04*t1
                   sinc21    = sin(c1)/c1
                   sum1      = g1*(sinc1+sinc21)
                   phif(i+1) = sinc1+sum1
                   t2        = real(i+2,kind=sp)
                   a2        = pi*f*t2
                   f2        = phi02*t2
                   sinc2     = f2*sin(a2)/a2
                   b2        = pi*fdif*t2
                   sinc12    = sin(b2)/b2
                   c2        = pi*fsum*t2
                   g2        = phi04*t2
                   sinc22    = sin(c2)/c2
                   sum2      = g2*(sinc2+sinc22)
                   phif(i+2) = sinc2+sum2
                   t3        = real(i+3,kind=sp)
                   a3        = pi*f*t3
                   f3        = phi02*t3
                   sinc3     = f3*sin(a3)/a3
                   b3        = pi*fdif*t3
                   sinc13    = sin(b3)/b3
                   c3        = pi*fsum*t3
                   g3        = phi04*t3
                   sinc23    = sin(c3)/c3
                   sum3      = g3*(sinc3+sinc23)
                   phif(i+3) = sinc3+sum3
                  
                  
               end do
           end if
       end subroutine raster_flux_mod_sinc_unroll_4x_r4



       subroutine raster_flux_mod_sinc_unroll_4x_r8(phif,htin,f,phi0,f0,rho0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_mod_sinc_unroll_4x_r8
           !dir$ attributes forceinline ::   raster_flux_mod_sinc_unroll_4x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_flux_mod_sinc_unroll_4x_r8
           real(kind=dp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=dp),                         intent(in)  :: f
           real(kind=dp),                         intent(in)  :: phi0
           real(kind=dp),                         intent(in)  :: f0
           real(kind=dp),                         intent(in)  :: rho0
           real(kind=dp), parameter :: pi = 3.14159265358979323846264338328_dp
           real(kind=dp), automatic :: sinc0,sinc1,sinc2,sinc3
           real(kind=dp), automatic :: sinc10,sinc11,sinc12,sinc13
           real(kind=dp), automatic :: sinc20,sinc21,sinc22,sinc23
           real(kind=dp), automatic :: a0,a1,a2,a3
           real(kind=dp), automatic :: b0,b1,b2,b3
           real(kind=dp), automatic :: t0,t1,t2,t3
           real(kind=dp), automatic :: c0,c1,c2,c3
           real(kind=dp), automatic :: sum0,sum1,sum2,sum3
           real(kind=dp), automatic :: f0,f1,f2,f3
           real(kind=dp), automatic :: g0,g1,g2,g3
           real(kind=dp), automatic :: fdif,fsum,phi02,phi04
           integer(kind=i4) :: i,m,m1,tin,nhtin
           tin = htin*2
           nhtin = -htin
           fdif  = f-f0
           fsum  = f+f0
           phi02 = 0.5_dp*phi0
           phi04 = 0.25_dp*phi0
           if(rho0/=0.5_dp) then
              call raster_flux_sinc_unroll_4x_r8(phif,htin,f,phi0)
              return
           else
              m = n(tin,4)
              if(m /= 0) then
                 do i=1,m
                    t0      = real(i,kind=dp)
                    a0      = pi*f*t0
                    f0      = phi02*t0
                    sinc0   = f0*sin(a0)/a0
                    b0      = pi*fdif*t0
                    sinc10  = sin(b0)/b0
                    c0      = pi*fsum*t0
                    g0      = phi04*t0
                    sinc20  = sin(c0)/c0
                    sum0    = g0*(sinc0+sinc20)
                   phif(i) = sinc0+sum0
                 end do
                 if(tin<4) return
              end if
              m1 = m+1
              ! Negative half first
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(8)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=nhtin,m,-4
                   t0        = real(i,kind=dp)
                   a0        = pi*f*t0
                   f0        = phi02*t0
                   sinc0     = f0*sin(a0)/a0
                   b0        = pi*fdif*t0
                   sinc10    = sin(b0)/b0
                   c0        = pi*fsum*t0
                   g0        = phi04*t0
                   sinc20    = sin(c0)/c0
                   sum0      = g0*(sinc0+sinc20)
                   phif(i)   = sinc0+sum0
                   t1        = real(i-1,kind=dp)
                   a1        = pi*f*t1
                   f1        = phi02*t1
                   sinc1     = f1*sin(a1)/a1
                   b1        = pi*fdif*t1
                   sinc11    = sin(b1)/b1
                   c1        = pi*fsum*t1
                   g1        = phi04*t1
                   sinc21    = sin(c1)/c1
                   sum1      = g1*(sinc1+sinc21)
                   phif(i-1) = sinc1+sum1
                   t2        = real(i-2,kind=dp)
                   a2        = pi*f*t2
                   f2        = phi02*t2
                   sinc2     = f2*sin(a2)/a2
                   b2        = pi*fdif*t2
                   sinc12    = sin(b2)/b2
                   c2        = pi*fsum*t2
                   g2        = phi04*t2
                   sinc22    = sin(c2)/c2
                   sum2      = g2*(sinc2+sinc22)
                   phif(i-2) = sinc2+sum2
                   t3        = real(i-3,kind=dp)
                   a3        = pi*f*t3
                   f3        = phi02*t3
                   sinc3     = f3*sin(a3)/a3
                   b3        = pi*fdif*t3
                   sinc13    = sin(b3)/b3
                   c3        = pi*fsum*t3
                   g3        = phi04*t3
                   sinc23    = sin(c3)/c3
                   sum3      = g3*(sinc3+sinc23)
                   phif(i-3) = sinc3+sum3
                  
              end do
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(8)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=m,htin,4
                   t0        = real(i,kind=dp)
                   a0        = pi*f*t0
                   f0        = phi02*t0
                   sinc0     = f0*sin(a0)/a0
                   b0        = pi*fdif*t0
                   sinc10    = sin(b0)/b0
                   c0        = pi*fsum*t0
                   g0        = phi04*t0
                   sinc20    = sin(c0)/c0
                   sum0      = g0*(sinc0+sinc20)
                   phif(i)   = sinc0+sum0
                   t1        = real(i+1,kind=dp)
                   a1        = pi*f*t1
                   f1        = phi02*t1
                   sinc1     = f1*sin(a1)/a1
                   b1        = pi*fdif*t1
                   sinc11    = sin(b1)/b1
                   c1        = pi*fsum*t1
                   g1        = phi04*t1
                   sinc21    = sin(c1)/c1
                   sum1      = g1*(sinc1+sinc21)
                   phif(i+1) = sinc1+sum1
                   t2        = real(i+2,kind=dp)
                   a2        = pi*f*t2
                   f2        = phi02*t2
                   sinc2     = f2*sin(a2)/a2
                   b2        = pi*fdif*t2
                   sinc12    = sin(b2)/b2
                   c2        = pi*fsum*t2
                   g2        = phi04*t2
                   sinc22    = sin(c2)/c2
                   sum2      = g2*(sinc2+sinc22)
                   phif(i+2) = sinc2+sum2
                   t3        = real(i+3,kind=dp)
                   a3        = pi*f*t3
                   f3        = phi02*t3
                   sinc3     = f3*sin(a3)/a3
                   b3        = pi*fdif*t3
                   sinc13    = sin(b3)/b3
                   c3        = pi*fsum*t3
                   g3        = phi04*t3
                   sinc23    = sin(c3)/c3
                   sum3      = g3*(sinc3+sinc23)
                   phif(i+3) = sinc3+sum3
                 
               end do
           end if
       end subroutine raster_flux_mod_sinc_unroll_4x_r8


       subroutine raster_flux_mod_sinc_unroll_2x_r4(phif,htin,f,phi0,f0,rho0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_mod_sinc_unroll_2x_r4
           !dir$ attributes forceinline ::   raster_flux_mod_sinc_unroll_2x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_flux_mod_sinc_unroll_2x_r4
           real(kind=sp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=sp),                         intent(in)  :: f
           real(kind=sp),                         intent(in)  :: phi0
           real(kind=sp),                         intent(in)  :: f0
           real(kind=sp),                         intent(in)  :: rho0
           real(kind=sp), parameter :: pi = 3.14159265358979323846264338328_sp
           real(kind=sp), automatic :: sinc0,sinc1
           real(kind=sp), automatic :: sinc10,sinc11
           real(kind=sp), automatic :: sinc20,sinc21
           real(kind=sp), automatic :: a0,a1
           real(kind=sp), automatic :: b0,b1
           real(kind=sp), automatic :: t0,t1
           real(kind=sp), automatic :: c0,c1
           real(kind=sp), automatic :: sum0,sum1
           real(kind=sp), automatic :: f0,f1
           real(kind=sp), automatic :: g0,g1
           real(kind=sp), automatic :: fdif,fsum,phi02,phi04
           integer(kind=i4) :: i,m,m1,tin,nhtin
           tin = htin*2
           nhtin = -htin
           fdif  = f-f0
           fsum  = f+f0
           phi02 = 0.5_sp*phi0
           phi04 = 0.25_sp*phi0
           if(rho0/=0.5_sp) then
              call raster_flux_sinc_unroll_2x_r4(phif,htin,f,phi0)
              return
           else
              m = n(tin,2)
              if(m /= 0) then
                 do i=1,m
                    t0      = real(i,kind=sp)
                    a0      = pi*f*t0
                    f0      = phi02*t0
                    sinc0   = f0*sin(a0)/a0
                    b0      = pi*fdif*t0
                    sinc10  = sin(b0)/b0
                    c0      = pi*fsum*t0
                    g0      = phi04*t0
                    sinc20  = sin(c0)/c0
                    sum0    = g0*(sinc0+sinc20)
                   phif(i) = sinc0+sum0
                 end do
                 if(tin<2) return
              end if
              m1 = m+1
              ! Negative half first
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(4)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=nhtin,m,-2
                   t0        = real(i,kind=sp)
                   a0        = pi*f*t0
                   f0        = phi02*t0
                   sinc0     = f0*sin(a0)/a0
                   b0        = pi*fdif*t0
                   sinc10    = sin(b0)/b0
                   c0        = pi*fsum*t0
                   g0        = phi04*t0
                   sinc20    = sin(c0)/c0
                   sum0      = g0*(sinc0+sinc20)
                   phif(i)   = sinc0+sum0
                   t1        = real(i-1,kind=sp)
                   a1        = pi*f*t1
                   f1        = phi02*t1
                   sinc1     = f1*sin(a1)/a1
                   b1        = pi*fdif*t1
                   sinc11    = sin(b1)/b1
                   c1        = pi*fsum*t1
                   g1        = phi04*t1
                   sinc21    = sin(c1)/c1
                   sum1      = g1*(sinc1+sinc21)
                   phif(i-1) = sinc1+sum1
              end do
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(4)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=m,htin,2
                   t0        = real(i,kind=sp)
                   a0        = pi*f*t0
                   f0        = phi02*t0
                   sinc0     = f0*sin(a0)/a0
                   b0        = pi*fdif*t0
                   sinc10    = sin(b0)/b0
                   c0        = pi*fsum*t0
                   g0        = phi04*t0
                   sinc20    = sin(c0)/c0
                   sum0      = g0*(sinc0+sinc20)
                   phif(i)   = sinc0+sum0
                   t1        = real(i+1,kind=sp)
                   a1        = pi*f*t1
                   f1        = phi02*t1
                   sinc1     = f1*sin(a1)/a1
                   b1        = pi*fdif*t1
                   sinc11    = sin(b1)/b1
                   c1        = pi*fsum*t1
                   g1        = phi04*t1
                   sinc21    = sin(c1)/c1
                   sum1      = g1*(sinc1+sinc21)
                   phif(i+1) = sinc1+sum1
                                  
                  
               end do
           end if
       end subroutine raster_flux_mod_sinc_unroll_2x_r4


       subroutine raster_flux_mod_sinc_unroll_2x_r8(phif,htin,f,phi0,f0,rho0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_mod_sinc_unroll_2x_r8
           !dir$ attributes forceinline ::   raster_flux_mod_sinc_unroll_2x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_flux_mod_sinc_unroll_2x_r8
           real(kind=dp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=dp),                         intent(in)  :: f
           real(kind=dp),                         intent(in)  :: phi0
           real(kind=dp),                         intent(in)  :: f0
           real(kind=dp),                         intent(in)  :: rho0
           real(kind=dp), parameter :: pi = 3.14159265358979323846264338328_dp
           real(kind=dp), automatic :: sinc0,sinc1
           real(kind=dp), automatic :: sinc10,sinc11
           real(kind=dp), automatic :: sinc20,sinc21
           real(kind=dp), automatic :: a0,a1
           real(kind=dp), automatic :: b0,b1
           real(kind=dp), automatic :: t0,t1
           real(kind=dp), automatic :: c0,c1
           real(kind=dp), automatic :: sum0,sum1
           real(kind=dp), automatic :: f0,f1
           real(kind=dp), automatic :: g0,g1
           real(kind=dp), automatic :: fdif,fsum,phi02,phi04
           integer(kind=i4) :: i,m,m1,tin,nhtin
           tin = htin*2
           nhtin = -htin
           fdif  = f-f0
           fsum  = f+f0
           phi02 = 0.5_dp*phi0
           phi04 = 0.25_dp*phi0
           if(rho0/=0.5_dp) then
              call raster_flux_sinc_unroll_2x_r8(phif,htin,f,phi0)
              return
           else
              m = n(tin,2)
              if(m /= 0) then
                 do i=1,m
                    t0      = real(i,kind=dp)
                    a0      = pi*f*t0
                    f0      = phi02*t0
                    sinc0   = f0*sin(a0)/a0
                    b0      = pi*fdif*t0
                    sinc10  = sin(b0)/b0
                    c0      = pi*fsum*t0
                    g0      = phi04*t0
                    sinc20  = sin(c0)/c0
                    sum0    = g0*(sinc0+sinc20)
                   phif(i) = sinc0+sum0
                 end do
                 if(tin<2) return
              end if
              m1 = m+1
              ! Negative half first
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(8)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=nhtin,m,-2
                   t0        = real(i,kind=dp)
                   a0        = pi*f*t0
                   f0        = phi02*t0
                   sinc0     = f0*sin(a0)/a0
                   b0        = pi*fdif*t0
                   sinc10    = sin(b0)/b0
                   c0        = pi*fsum*t0
                   g0        = phi04*t0
                   sinc20    = sin(c0)/c0
                   sum0      = g0*(sinc0+sinc20)
                   phif(i)   = sinc0+sum0
                   t1        = real(i-1,kind=dp)
                   a1        = pi*f*t1
                   f1        = phi02*t1
                   sinc1     = f1*sin(a1)/a1
                   b1        = pi*fdif*t1
                   sinc11    = sin(b1)/b1
                   c1        = pi*fsum*t1
                   g1        = phi04*t1
                   sinc21    = sin(c1)/c1
                   sum1      = g1*(sinc1+sinc21)
                   phif(i-1) = sinc1+sum1
              end do
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(8)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=m,htin,2
                   t0        = real(i,kind=dp)
                   a0        = pi*f*t0
                   f0        = phi02*t0
                   sinc0     = f0*sin(a0)/a0
                   b0        = pi*fdif*t0
                   sinc10    = sin(b0)/b0
                   c0        = pi*fsum*t0
                   g0        = phi04*t0
                   sinc20    = sin(c0)/c0
                   sum0      = g0*(sinc0+sinc20)
                   phif(i)   = sinc0+sum0
                   t1        = real(i+1,kind=dp)
                   a1        = pi*f*t1
                   f1        = phi02*t1
                   sinc1     = f1*sin(a1)/a1
                   b1        = pi*fdif*t1
                   sinc11    = sin(b1)/b1
                   c1        = pi*fsum*t1
                   g1        = phi04*t1
                   sinc21    = sin(c1)/c1
                   sum1      = g1*(sinc1+sinc21)
                   phif(i+1) = sinc1+sum1
                                   
               end do
           end if
       end subroutine raster_flux_mod_sinc_unroll_2x_r8


       subroutine raster_flux_mod_sinc_r4(phif,htin,f,phi0,f0,rho0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_mod_sinc_r4
           !dir$ attributes forceinline ::   raster_flux_mod_sinc_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_flux_mod_sinc_r4
           real(kind=sp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=sp),                         intent(in)  :: f
           real(kind=sp),                         intent(in)  :: phi0
           real(kind=sp),                         intent(in)  :: f0
           real(kind=sp),                         intent(in)  :: rho0
           real(kind=sp), parameter :: pi = 3.14159265358979323846264338328_sp
           real(kind=sp), automatic :: sinc0
           real(kind=sp), automatic :: sinc10
           real(kind=sp), automatic :: sinc20
           real(kind=sp), automatic :: a0
           real(kind=sp), automatic :: b0
           real(kind=sp), automatic :: t0
           real(kind=sp), automatic :: c0
           real(kind=sp), automatic :: sum0
           real(kind=sp), automatic :: f0
           real(kind=sp), automatic :: g0
           real(kind=sp), automatic :: fdif,fsum,phi02,phi04
           integer(kind=i4) :: i,tin,nhtin
           tin = htin*2
           nhtin = -htin
           fdif  = f-f0
           fsum  = f+f0
           phi02 = 0.5_sp*phi0
           phi04 = 0.25_sp*phi0
           if(rho0/=0.5_sp) then
              call raster_flux_sinc_r4(phif,htin,f,phi0)
              return
           else
             
              ! Negative half first
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(4)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=nhtin,0
                   t0        = real(i,kind=sp)
                   a0        = pi*f*t0
                   f0        = phi02*t0
                   sinc0     = f0*sin(a0)/a0
                   b0        = pi*fdif*t0
                   sinc10    = sin(b0)/b0
                   c0        = pi*fsum*t0
                   g0        = phi04*t0
                   sinc20    = sin(c0)/c0
                   sum0      = g0*(sinc0+sinc20)
                   phif(i)   = sinc0+sum0
              end do
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(4)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=1,htin
                   t0        = real(i,kind=sp)
                   a0        = pi*f*t0
                   f0        = phi02*t0
                   sinc0     = f0*sin(a0)/a0
                   b0        = pi*fdif*t0
                   sinc10    = sin(b0)/b0
                   c0        = pi*fsum*t0
                   g0        = phi04*t0
                   sinc20    = sin(c0)/c0
                   sum0      = g0*(sinc0+sinc20)
                   phif(i)   = sinc0+sum0
                                                    
               end do
           end if
       end subroutine raster_flux_mod_sinc_r4


       subroutine raster_flux_mod_sinc_r8(phif,htin,f,phi0,f0,rho0)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_flux_mod_sinc_r8
           !dir$ attributes forceinline ::   raster_flux_mod_sinc_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_flux_mod_sinc_r8
           real(kind=dp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=dp),                         intent(in)  :: f
           real(kind=dp),                         intent(in)  :: phi0
           real(kind=dp),                         intent(in)  :: f0
           real(kind=dp),                         intent(in)  :: rho0
           real(kind=dp), parameter :: pi = 3.14159265358979323846264338328_dp
           real(kind=dp), automatic :: sinc0
           real(kind=dp), automatic :: sinc10
           real(kind=dp), automatic :: sinc20
           real(kind=dp), automatic :: a0
           real(kind=dp), automatic :: b0
           real(kind=dp), automatic :: t0
           real(kind=dp), automatic :: c0
           real(kind=dp), automatic :: sum0
           real(kind=dp), automatic :: f0
           real(kind=dp), automatic :: g0
           real(kind=dp), automatic :: fdif,fsum,phi02,phi04
           integer(kind=i4) :: i,tin,nhtin
           tin = htin*2
           nhtin = -htin
           fdif  = f-f0
           fsum  = f+f0
           phi02 = 0.5_dp*phi0
           phi04 = 0.25_dp*phi0
           if(rho0/=0.5_dp) then
              call raster_flux_sinc_r8(phif,htin,f,phi0)
              return
           else
             
              ! Negative half first
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(8)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=nhtin,0
                   t0        = real(i,kind=dp)
                   a0        = pi*f*t0
                   f0        = phi02*t0
                   sinc0     = f0*sin(a0)/a0
                   b0        = pi*fdif*t0
                   sinc10    = sin(b0)/b0
                   c0        = pi*fsum*t0
                   g0        = phi04*t0
                   sinc20    = sin(c0)/c0
                   sum0      = g0*(sinc0+sinc20)
                   phif(i)   = sinc0+sum0
              end do
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(8)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=1,htin
                   t0        = real(i,kind=dp)
                   a0        = pi*f*t0
                   f0        = phi02*t0
                   sinc0     = f0*sin(a0)/a0
                   b0        = pi*fdif*t0
                   sinc10    = sin(b0)/b0
                   c0        = pi*fsum*t0
                   g0        = phi04*t0
                   sinc20    = sin(c0)/c0
                   sum0      = g0*(sinc0+sinc20)
                   phif(i)   = sinc0+sum0
                                                    
               end do
           end if
       end subroutine raster_flux_mod_sinc_r8


       subroutine raster_flux_mod_sinc_exec_r4(phif,htin,f,phi0,f0,rho0,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: raster_flux_mod_sinc_exec_r4
           real(kind=sp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=sp),                         intent(in)  :: f
           real(kind=sp),                         intent(in)  :: phi0
           real(kind=sp),                         intent(in)  :: f0
           real(kind=sp),                         intent(in)  :: rho0
           integer(kind=i4),                      intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                 call raster_flux_mod_sinc_unroll_16x_r4(phif,htin,f,phi0,f0,rho0)
              case (8)
                 call raster_flux_mod_sinc_unroll_8x_r4(phif,htin,f,phi0,f0,rho0)
              case (4)
                 call raster_flux_mod_sinc_unroll_4x_r4(phif,htin,f,phi0,f0,rho0)
              case (2)
                 call raster_flux_mod_sinc_unroll_2x_r4(phif,htin,f,phi0,f0,rho0)
              case (0)
                 call raster_flux_mod_sinc_r4(phif,htin,f,phi0,f0,rho0)
              case default
                 return
              end select 
       end subroutine raster_flux_mod_sinc_exec_r4


       subroutine raster_flux_mod_sinc_exec_r8(phif,htin,f,phi0,f0,rho0,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: raster_flux_mod_sinc_exec_r8
           real(kind=dp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=dp),                         intent(in)  :: f
           real(kind=dp),                         intent(in)  :: phi0
           real(kind=dp),                         intent(in)  :: f0
           real(kind=dp),                         intent(in)  :: rho0
           integer(kind=i4),                      intent(in)  :: unroll_cnt
           select case (unroll_cnt)
              case (16)
                 call raster_flux_mod_sinc_unroll_16x_r8(phif,htin,f,phi0,f0,rho0)
              case (8)
                 call raster_flux_mod_sinc_unroll_8x_r8(phif,htin,f,phi0,f0,rho0)
              case (4)
                 call raster_flux_mod_sinc_unroll_4x_r8(phif,htin,f,phi0,f0,rho0)
              case (2)
                 call raster_flux_mod_sinc_unroll_2x_r8(phif,htin,f,phi0,f0,rho0)
              case (0)
                 call raster_flux_mod_sinc_r8(phif,htin,f,phi0,f0,rho0)
              case default
                 return
              end select 
       end subroutine raster_flux_mod_sinc_exec_r8


       !Если момент времени, соответствующий центру импульса 
       !падающего потока излучения, сдвинут относительно момента 
       !времени, соответствующего максимуму пропускания растра, на 
       !величину Д£, то спектр модулированного излучения окажется равным
       !Formula (the last one), p. 202
       subroutine raster_mod_sinc_shifted_unroll_16x_r4(phif,htin,f,phi0,fx,rho0,dt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_mod_sinc_shifted_unroll_16x_r4
           !dir$ attributes forceinline ::   raster_mod_sinc_shifted_unroll_16x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_mod_sinc_shifted_unroll_16x_r4
           real(kind=sp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=sp),                         intent(in)  :: f
           real(kind=sp),                         intent(in)  :: phi0
           real(kind=sp),                         intent(in)  :: fx
           real(kind=sp),                         intent(in)  :: rho0
           real(kind=sp),                         intent(in)  :: dt
           real(kind=sp), parameter :: pi    = 3.14159265358979323846264338328_sp
           real(kind=sp), parameter :: twopi = 2.0_sp*pi
          
           real(kind=sp), automatic :: sinc0,sinc1,sinc2,sinc3,sinc4,sinc5,sinc6,sinc7
           real(kind=sp), automatic :: sinc8,sinc9,sinc10,sinc11,sinc12,sinc13,sinc14,sinc15
           real(kind=sp), automatic :: sinc10,sinc11,sinc12,sinc13,sinc14,sinc15,sinc16,sinc17
           real(kind=sp), automatic :: sinc18,sinc19,sinc110,sinc111,sinc112,sinc113,sinc114,sinc115
           real(kind=sp), automatic :: sinc20,sinc21,sinc22,sinc23,sinc24,sinc25,sinc26,sinc27
           real(kind=sp), automatic :: sinc28,sinc29,sinc210,sinc211,sinc212,sinc213,sinc214,sinc215
           real(kind=sp), automatic :: cos0,cos1,cos2,cos3,cos4,cos5,cos6,cos7
           real(kind=sp), automatic :: cos8,cos9,cos10,cos11,cos12,cos13,cos14,cos15
           real(kind=sp), automatic :: cos10,cos11,cos12,cos13,cos14,cos15,cos16,cos17
           real(kind=sp), automatic :: cos18,cos19,cos110,cos111,cos112,cos113,cos114,cos115
            real(kind=sp), automatic :: a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15
           real(kind=sp), automatic :: b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15
           real(kind=sp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15
           real(kind=sp), automatic :: c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15
           real(kind=sp), automatic :: sum0,sum1,sum2,sum3,sum4,sum5,sum6,sum7
           real(kind=sp), automatic :: sum8,sum9,sum10,sum11,sum12,sum13,sum14,sum15
           real(kind=sp), automatic :: f0,f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15
           real(kind=sp), automatic :: g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15
           real(kind=sp), automatic :: dt0,dt1,dt2,dt3,dt4,dt5,dt6,dt7
           real(kind=sp), automatic :: dt8,dt9,dt10,dt11,dt12,dt13,dt14,dt15
           real(kind=sp), automatic :: fdif,fsum,phi02,phi04,twopif,twopif0
           integer(kind=i4) :: i,m,m1,tin,nhtin
           tin    = htin*2
           nhtin  = -htin
           fdif   = f-fx
           fsum   = f+fx
           phi02  = 0.5_sp*phi0
           phi04  = 0.25_sp*phi0
           twopif = twopi*f
           twopif0=twopi*fx
           if(rho0/=0.5_sp) then
              call raster_flux_sinc_unroll_16x_r4(phif,htin,f,phi0)
              return
           else
              m = mod(tin,16)
              if(m /= 0) then
                 do i=1,m
                    t0      = real(i,kind=sp)
                    dt0     = dt+t0
                    a0      = pi*f*t0
                    f0      = phi02*t0
                    sinc0   = f0*sin(a0)/a0
                    cos0    = cos(twopif0*dt0)
                    b0      = pi*fdif*t0
                    sinc10  = cos0*sin(b0)/b0
                    c0      = pi*fsum*t0
                    g0      = phi04*t0
                    cos10   = cos(-twopif0*dt0)
                    sinc20  = cos10*sin(c0)/c0
                    sum0    = g0*(sinc0+sinc20)
                    phif(i) = (sinc0+sum0)*cos(-twopif*dt0)
                 end do
                 if(tin<16) return
              end if
              m1 = m+1
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(4)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=nhtin,m,16
                    t0        = real(i,kind=sp)
                    dt0       = dt+t0
                    a0        = pi*f*t0
                    f0        = phi02*t0
                    sinc0     = f0*sin(a0)/a0
                    cos0      = cos(twopif0*dt0)
                    b0        = pi*fdif*t0
                    sinc10    = cos0*sin(b0)/b0
                    c0        = pi*fsum*t0
                    g0        = phi04*t0
                    cos10     = cos(-twopif0*dt0)
                    sinc20    = cos10*sin(c0)/c0
                    sum0      = g0*(sinc0+sinc20)
                    phif(i)   = (sinc0+sum0)*cos(-twopif*dt0)
                    t1        = real(i-1,kind=sp)
                    dt1       = dt+t1
                    a1        = pi*f*t1
                    f1        = phi02*t1
                    sinc1     = f1*sin(a1)/a1
                    cos1      = cos(twopif0*dt1)
                    b1        = pi*fdif*t1
                    sinc11    = cos1*sin(b1)/b1
                    c1        = pi*fsum*t1
                    g1        = phi04*t1
                    cos11     = cos(-twopif0*dt1)
                    sinc21    = cos11*sin(c1)/c1
                    sum1      = g1*(sinc1+sinc21)
                    phif(i-1) = (sinc1+sum1)*cos(-twopif*dt1)
                    t2        = real(i-2,kind=sp)
                    dt2       = dt+t2
                    a2        = pi*f*t2
                    f2        = phi02*t2
                    sinc2     = f2*sin(a2)/a2
                    cos2      = cos(twopif0*dt2)
                    b2        = pi*fdif*t2
                    sinc12    = cos2*sin(b2)/b2
                    c2        = pi*fsum*t2
                    g2        = phi04*t2
                    cos12     = cos(-twopif0*dt2)
                    sinc22    = cos12*sin(c2)/c2
                    sum2      = g2*(sinc2+sinc22)
                    phif(i-2) = (sinc2+sum2)*cos(-twopif*dt2)
                    t3        = real(i-3,kind=sp)
                    dt3       = dt+t3
                    a3        = pi*f*t3
                    f3        = phi02*t3
                    sinc3     = f3*sin(a3)/a3
                    cos3      = cos(twopif0*dt3)
                    b3        = pi*fdif*t3
                    sinc13    = cos3*sin(b3)/b3
                    c3        = pi*fsum*t3
                    g3        = phi04*t3
                    cos13     = cos(-twopif0*dt3)
                    sinc23    = cos13*sin(c3)/c3
                    sum3      = g3*(sinc3+sinc23)
                    phif(i-3) = (sinc3+sum3)*cos(-twopif*dt3)
                    t4        = real(i-4,kind=sp)
                    dt4       = dt+t4
                    a4        = pi*f*t4
                    f4        = phi02*t4
                    sinc4     = f4*sin(a4)/a4
                    cos4      = cos(twopif0*dt4)
                    b4        = pi*fdif*t4
                    sinc14    = cos4*sin(b4)/b4
                    c4        = pi*fsum*t4
                    g4        = phi04*t4
                    cos14     = cos(-twopif0*dt4)
                    sinc24    = cos14*sin(c4)/c4
                    sum4      = g4*(sinc4+sinc24)
                    phif(i-4) = (sinc4+sum4)*cos(-twopif*dt4)
                    t5        = real(i-5,kind=sp)
                    dt5       = dt+t5
                    a5        = pi*f*t5
                    f5        = phi02*t5
                    sinc5     = f5*sin(a5)/a5
                    cos5      = cos(twopif0*dt5)
                    b5        = pi*fdif*t5
                    sinc15    = cos5*sin(b5)/b5
                    c5        = pi*fsum*t5
                    g5        = phi04*t5
                    cos15     = cos(-twopif0*dt5)
                    sinc25    = cos15*sin(c5)/c5
                    sum5      = g5*(sinc5+sinc25)
                    phif(i-5) = (sinc5+sum5)*cos(-twopif*dt5)
                    t6        = real(i-6,kind=sp)
                    dt6       = dt+t6
                    a6        = pi*f*t6
                    f6        = phi02*t6
                    sinc6     = f6*sin(a6)/a6
                    cos6      = cos(twopif0*dt6)
                    b6        = pi*fdif*t6
                    sinc16    = cos6*sin(b6)/b6
                    c6        = pi*fsum*t6
                    g6        = phi04*t6
                    cos16     = cos(-twopif0*dt6)
                    sinc26    = cos16*sin(c6)/c6
                    sum6      = g6*(sinc6+sinc26)
                    phif(i-6) = (sinc6+sum6)*cos(-twopif*dt6)
                    t7        = real(i-7,kind=sp)
                    dt7       = dt+t7
                    a7        = pi*f*t7
                    f7        = phi02*t7
                    sinc7     = f7*sin(a7)/a7
                    cos7      = cos(twopif0*dt7)
                    b7        = pi*fdif*t7
                    sinc17    = cos7*sin(b7)/b7
                    c7        = pi*fsum*t7
                    g7        = phi04*t7
                    cos17     = cos(-twopif0*dt7)
                    sinc27    = cos17*sin(c7)/c7
                    sum7      = g7*(sinc7+sinc27)
                    phif(i-7) = (sinc7+sum7)*cos(-twopif*dt7)
                    t8        = real(i-8,kind=sp)
                    dt8       = dt+t8
                    a8        = pi*f*t8
                    f8        = phi02*t8
                    sinc8     = f8*sin(a8)/a8
                    cos8      = cos(twopif0*dt8)
                    b8        = pi*fdif*t8
                    sinc18    = cos8*sin(b8)/b8
                    c8        = pi*fsum*t8
                    g8        = phi04*t8
                    cos18     = cos(-twopif0*dt8)
                    sinc28    = cos18*sin(c8)/c8
                    sum8      = g8*(sinc8+sinc28)
                    phif(i-8) = (sinc8+sum8)*cos(-twopif*dt8) 
                    t9        = real(i-9,kind=sp)
                    dt9       = dt+t9
                    a9        = pi*f*t9
                    f9        = phi02*t9
                    sinc9     = f9*sin(a9)/a9
                    cos9      = cos(twopif0*dt9)
                    b9        = pi*fdif*t9
                    sinc19    = cos1*sin(b9)/b9
                    c9        = pi*fsum*t9
                    g9        = phi04*t9
                    cos19     = cos(-twopif0*dt9)
                    sinc29    = cos19*sin(c9)/c9
                    sum9      = g9*(sinc9+sinc29)
                    phif(i-9) = (sinc9+sum9)*cos(-twopif*dt9)
                    t10       = real(i-10,kind=sp)
                    dt10      = dt+t10
                    a10       = pi*f*t10
                    f10       = phi02*t10
                    sinc10    = f10*sin(a10)/a10
                    cos10     = cos(twopif0*dt10)
                    b10       = pi*fdif*t10
                    sinc110   = cos10*sin(b10)/b10
                    c10       = pi*fsum*t10
                    g10       = phi04*t10
                    cos110    = cos(-twopif0*dt10)
                    sinc210   = cos110*sin(c10)/c10
                    sum10     = g10*(sinc10+sinc210)
                    phif(i-10)= (sinc10+sum10)*cos(-twopif*dt10)
                    t11       = real(i-11,kind=sp)
                    dt11      = dt+t11
                    a11       = pi*f*t11
                    f11       = phi02*t11
                    sinc11    = f11*sin(a11)/a11
                    cos11     = cos(twopif0*dt11)
                    b11       = pi*fdif*t11
                    sinc111   = cos11*sin(b11)/b11
                    c11       = pi*fsum*t11
                    g11       = phi04*t11
                    cos111    = cos(-twopif0*dt11)
                    sinc211   = cos111*sin(c11)/c11
                    sum11     = g11*(sinc11+sinc211)
                    phif(i-11)= (sinc11+sum11)*cos(-twopif*dt11)
                    t12       = real(i-12,kind=sp)
                    dt12      = dt+t12
                    a12       = pi*f*t12
                    f12       = phi02*t12
                    sinc12    = f12*sin(a12)/a12
                    cos12     = cos(twopif0*dt12)
                    b12       = pi*fdif*t12
                    sinc112   = cos12*sin(b12)/b12
                    c12       = pi*fsum*t12
                    g12       = phi04*t12
                    cos12     = cos(-twopif0*dt12)
                    sinc212   = cos12*sin(c12)/c12
                    sum12     = g12*(sinc12+sinc212)
                    phif(i-12)= (sinc12+sum12)*cos(-twopif*dt12)
                    t13       = real(i-13,kind=sp)
                    dt13      = dt+t13
                    a13       = pi*f*t13
                    f13       = phi02*t13
                    sinc13    = f13*sin(a13)/a13
                    cos13     = cos(twopif0*dt13)
                    b13       = pi*fdif*t13
                    sinc113   = cos13*sin(b13)/b13
                    c13       = pi*fsum*t13
                    g13       = phi04*t13
                    cos113    = cos(-twopif0*dt13)
                    sinc213   = cos13*sin(c13)/c13
                    sum13     = g13*(sinc13+sinc213)
                    phif(i-13)= (sinc13+sum13)*cos(-twopif*dt13)
                    t14       = real(i-14,kind=sp)
                    dt14      = dt+t14
                    a14       = pi*f*t14
                    f14       = phi02*t14
                    sinc14    = f14*sin(a14)/a14
                    cos14     = cos(twopif0*dt14)
                    b14       = pi*fdif*t14
                    sinc114   = cos14*sin(b14)/b14
                    c14       = pi*fsum*t14
                    g14       = phi04*t14
                    cos114    = cos(-twopif0*dt14)
                    sinc214   = cos14*sin(c14)/c14
                    sum14     = g14*(sinc14+sinc214)
                    phif(i-14)= (sinc14+sum14)*cos(-twopif*dt14)
                    t15       = real(i-15,kind=sp)
                    dt15      = dt+t15
                    a15       = pi*f*t15
                    f15       = phi02*t15
                    sinc15    = f15*sin(a15)/a15
                    cos15     = cos(twopif0*dt15)
                    b15       = pi*fdif*t15
                    sinc115   = cos15*sin(b15)/b15
                    c15       = pi*fsum*t15
                    g15       = phi04*t15
                    cos115    = cos(-twopif0*dt15)
                    sinc215   = cos15*sin(c15)/c15
                    sum15     = g15*(sinc15+sinc215)
                    phif(i-15)= (sinc15+sum15)*cos(-twopif*dt15)
              end do
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(4)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=m,htin,16
                    t0        = real(i,kind=sp)
                    dt0       = dt+t0
                    a0        = pi*f*t0
                    f0        = phi02*t0
                    sinc0     = f0*sin(a0)/a0
                    cos0      = cos(twopif0*dt0)
                    b0        = pi*fdif*t0
                    sinc10    = cos0*sin(b0)/b0
                    c0        = pi*fsum*t0
                    g0        = phi04*t0
                    cos10     = cos(-twopif0*dt0)
                    sinc20    = cos10*sin(c0)/c0
                    sum0      = g0*(sinc0+sinc20)
                    phif(i)   = (sinc0+sum0)*cos(-twopif*dt0)
                    t1        = real(i+1,kind=sp)
                    dt1       = dt+t1
                    a1        = pi*f*t1
                    f1        = phi02*t1
                    sinc1     = f1*sin(a1)/a1
                    cos1      = cos(twopif0*dt1)
                    b1        = pi*fdif*t1
                    sinc11    = cos1*sin(b1)/b1
                    c1        = pi*fsum*t1
                    g1        = phi04*t1
                    cos11     = cos(-twopif0*dt1)
                    sinc21    = cos11*sin(c1)/c1
                    sum1      = g1*(sinc1+sinc21)
                    phif(i+1) = (sinc1+sum1)*cos(-twopif*dt1)
                    t2        = real(i+2,kind=sp)
                    dt2       = dt+t2
                    a2        = pi*f*t2
                    f2        = phi02*t2
                    sinc2     = f2*sin(a2)/a2
                    cos2      = cos(twopif0*dt2)
                    b2        = pi*fdif*t2
                    sinc12    = cos2*sin(b2)/b2
                    c2        = pi*fsum*t2
                    g2        = phi04*t2
                    cos12     = cos(-twopif0*dt2)
                    sinc22    = cos12*sin(c2)/c2
                    sum2      = g2*(sinc2+sinc22)
                    phif(i+2) = (sinc2+sum2)*cos(-twopif*dt2)
                    t3        = real(i+3,kind=sp)
                    dt3       = dt+t3
                    a3        = pi*f*t3
                    f3        = phi02*t3
                    sinc3     = f3*sin(a3)/a3
                    cos3      = cos(twopif0*dt3)
                    b3        = pi*fdif*t3
                    sinc13    = cos3*sin(b3)/b3
                    c3        = pi*fsum*t3
                    g3        = phi04*t3
                    cos13     = cos(-twopif0*dt3)
                    sinc23    = cos13*sin(c3)/c3
                    sum3      = g3*(sinc3+sinc23)
                    phif(i+3) = (sinc3+sum3)*cos(-twopif*dt3)
                    t4        = real(i+4,kind=sp)
                    dt4       = dt+t4
                    a4        = pi*f*t4
                    f4        = phi02*t4
                    sinc4     = f4*sin(a4)/a4
                    cos4      = cos(twopif0*dt4)
                    b4        = pi*fdif*t4
                    sinc14    = cos4*sin(b4)/b4
                    c4        = pi*fsum*t4
                    g4        = phi04*t4
                    cos14     = cos(-twopif0*dt4)
                    sinc24    = cos14*sin(c4)/c4
                    sum4      = g4*(sinc4+sinc24)
                    phif(i+4) = (sinc4+sum4)*cos(-twopif*dt4)
                    t5        = real(i+5,kind=sp)
                    dt5       = dt+t5
                    a5        = pi*f*t5
                    f5        = phi02*t5
                    sinc5     = f5*sin(a5)/a5
                    cos5      = cos(twopif0*dt5)
                    b5        = pi*fdif*t5
                    sinc15    = cos5*sin(b5)/b5
                    c5        = pi*fsum*t5
                    g5        = phi04*t5
                    cos15     = cos(-twopif0*dt5)
                    sinc25    = cos15*sin(c5)/c5
                    sum5      = g5*(sinc5+sinc25)
                    phif(i+5) = (sinc5+sum5)*cos(-twopif*dt5)
                    t6        = real(i+6,kind=sp)
                    dt6       = dt+t6
                    a6        = pi*f*t6
                    f6        = phi02*t6
                    sinc6     = f6*sin(a6)/a6
                    cos6      = cos(twopif0*dt6)
                    b6        = pi*fdif*t6
                    sinc16    = cos6*sin(b6)/b6
                    c6        = pi*fsum*t6
                    g6        = phi04*t6
                    cos16     = cos(-twopif0*dt6)
                    sinc26    = cos16*sin(c6)/c6
                    sum6      = g6*(sinc6+sinc26)
                    phif(i+6) = (sinc6+sum6)*cos(-twopif*dt6)
                    t7        = real(i+7,kind=sp)
                    dt7       = dt+t7
                    a7        = pi*f*t7
                    f7        = phi02*t7
                    sinc7     = f7*sin(a7)/a7
                    cos7      = cos(twopif0*dt7)
                    b7        = pi*fdif*t7
                    sinc17    = cos7*sin(b7)/b7
                    c7        = pi*fsum*t7
                    g7        = phi04*t7
                    cos17     = cos(-twopif0*dt7)
                    sinc27    = cos17*sin(c7)/c7
                    sum7      = g7*(sinc7+sinc27)
                    phif(i+7) = (sinc7+sum7)*cos(-twopif*dt7)
                    t8        = real(i+8,kind=sp)
                    dt8       = dt+t8
                    a8        = pi*f*t8
                    f8        = phi02*t8
                    sinc8     = f8*sin(a8)/a8
                    cos8      = cos(twopif0*dt8)
                    b8        = pi*fdif*t8
                    sinc18    = cos8*sin(b8)/b8
                    c8        = pi*fsum*t8
                    g8        = phi04*t8
                    cos18     = cos(-twopif0*dt8)
                    sinc28    = cos18*sin(c8)/c8
                    sum8      = g8*(sinc8+sinc28)
                    phif(i+8) = (sinc8+sum8)*cos(-twopif*dt8) 
                    t9        = real(i+9,kind=sp)
                    dt9       = dt+t9
                    a9        = pi*f*t9
                    f9        = phi02*t9
                    sinc9     = f9*sin(a9)/a9
                    cos9      = cos(twopif0*dt9)
                    b9        = pi*fdif*t9
                    sinc19    = cos1*sin(b9)/b9
                    c9        = pi*fsum*t9
                    g9        = phi04*t9
                    cos19     = cos(-twopif0*dt9)
                    sinc29    = cos19*sin(c9)/c9
                    sum9      = g9*(sinc9+sinc29)
                    phif(i+9) = (sinc9+sum9)*cos(-twopif*dt9)
                    t10       = real(i+10,kind=sp)
                    dt10      = dt+t10
                    a10       = pi*f*t10
                    f10       = phi02*t10
                    sinc10    = f10*sin(a10)/a10
                    cos10     = cos(twopif0*dt10)
                    b10       = pi*fdif*t10
                    sinc110   = cos10*sin(b10)/b10
                    c10       = pi*fsum*t10
                    g10       = phi04*t10
                    cos110    = cos(-twopif0*dt10)
                    sinc210   = cos110*sin(c10)/c10
                    sum10     = g10*(sinc10+sinc210)
                    phif(i+10)= (sinc10+sum10)*cos(-twopif*dt10)
                    t11       = real(i+11,kind=sp)
                    dt11      = dt+t11
                    a11       = pi*f*t11
                    f11       = phi02*t11
                    sinc11    = f11*sin(a11)/a11
                    cos11     = cos(twopif0*dt11)
                    b11       = pi*fdif*t11
                    sinc111   = cos11*sin(b11)/b11
                    c11       = pi*fsum*t11
                    g11       = phi04*t11
                    cos111    = cos(-twopif0*dt11)
                    sinc211   = cos111*sin(c11)/c11
                    sum11     = g11*(sinc11+sinc211)
                    phif(i+11)= (sinc11+sum11)*cos(-twopif*dt11)
                    t12       = real(i+12,kind=sp)
                    dt12      = dt+t12
                    a12       = pi*f*t12
                    f12       = phi02*t12
                    sinc12    = f12*sin(a12)/a12
                    cos12     = cos(twopif0*dt12)
                    b12       = pi*fdif*t12
                    sinc112   = cos12*sin(b12)/b12
                    c12       = pi*fsum*t12
                    g12       = phi04*t12
                    cos12     = cos(-twopif0*dt12)
                    sinc212   = cos12*sin(c12)/c12
                    sum12     = g12*(sinc12+sinc212)
                    phif(i+12)= (sinc12+sum12)*cos(-twopif*dt12)
                    t13       = real(i+13,kind=sp)
                    dt13      = dt+t13
                    a13       = pi*f*t13
                    f13       = phi02*t13
                    sinc13    = f13*sin(a13)/a13
                    cos13     = cos(twopif0*dt13)
                    b13       = pi*fdif*t13
                    sinc113   = cos13*sin(b13)/b13
                    c13       = pi*fsum*t13
                    g13       = phi04*t13
                    cos113    = cos(-twopif0*dt13)
                    sinc213   = cos13*sin(c13)/c13
                    sum13     = g13*(sinc13+sinc213)
                    phif(i+13)= (sinc13+sum13)*cos(-twopif*dt13)
                    t14       = real(i+14,kind=sp)
                    dt14      = dt+t14
                    a14       = pi*f*t14
                    f14       = phi02*t14
                    sinc14    = f14*sin(a14)/a14
                    cos14     = cos(twopif0*dt14)
                    b14       = pi*fdif*t14
                    sinc114   = cos14*sin(b14)/b14
                    c14       = pi*fsum*t14
                    g14       = phi04*t14
                    cos114    = cos(-twopif0*dt14)
                    sinc214   = cos14*sin(c14)/c14
                    sum14     = g14*(sinc14+sinc214)
                    phif(i+14)= (sinc14+sum14)*cos(-twopif*dt14)
                    t15       = real(i+15,kind=sp)
                    dt15      = dt+t15
                    a15       = pi*f*t15
                    f15       = phi02*t15
                    sinc15    = f15*sin(a15)/a15
                    cos15     = cos(twopif0*dt15)
                    b15       = pi*fdif*t15
                    sinc115   = cos15*sin(b15)/b15
                    c15       = pi*fsum*t15
                    g15       = phi04*t15
                    cos115    = cos(-twopif0*dt15)
                    sinc215   = cos15*sin(c15)/c15
                    sum15     = g15*(sinc15+sinc215)
                    phif(i+15)= (sinc15+sum15)*cos(-twopif*dt15)
              end do
           end if
       end subroutine raster_mod_sinc_shifted_unroll_16x_r4 


       subroutine raster_mod_sinc_shifted_unroll_16x_r8(phif,htin,f,phi0,fx,rho0,dt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_mod_sinc_shifted_unroll_16x_r8
           !dir$ attributes forceinline ::   raster_mod_sinc_shifted_unroll_16x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_mod_sinc_shifted_unroll_16x_r8
           real(kind=dp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=dp),                         intent(in)  :: f
           real(kind=dp),                         intent(in)  :: phi0
           real(kind=dp),                         intent(in)  :: fx
           real(kind=dp),                         intent(in)  :: rho0
           real(kind=dp),                         intent(in)  :: dt
           real(kind=dp), parameter :: pi    = 3.14159265358979323846264338328_dp
           real(kind=dp), parameter :: twopi = 2.0_dp*pi
          
           real(kind=dp), automatic :: sinc0,sinc1,sinc2,sinc3,sinc4,sinc5,sinc6,sinc7
           real(kind=dp), automatic :: sinc8,sinc9,sinc10,sinc11,sinc12,sinc13,sinc14,sinc15
           real(kind=dp), automatic :: sinc10,sinc11,sinc12,sinc13,sinc14,sinc15,sinc16,sinc17
           real(kind=dp), automatic :: sinc18,sinc19,sinc110,sinc111,sinc112,sinc113,sinc114,sinc115
           real(kind=dp), automatic :: sinc20,sinc21,sinc22,sinc23,sinc24,sinc25,sinc26,sinc27
           real(kind=dp), automatic :: sinc28,sinc29,sinc210,sinc211,sinc212,sinc213,sinc214,sinc215
           real(kind=dp), automatic :: cos0,cos1,cos2,cos3,cos4,cos5,cos6,cos7
           real(kind=dp), automatic :: cos8,cos9,cos10,cos11,cos12,cos13,cos14,cos15
           real(kind=dp), automatic :: cos10,cos11,cos12,cos13,cos14,cos15,cos16,cos17
           real(kind=dp), automatic :: cos18,cos19,cos110,cos111,cos112,cos113,cos114,cos115
           real(kind=dp), automatic :: a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15
           real(kind=dp), automatic :: b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15
           real(kind=dp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15
           real(kind=dp), automatic :: c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15
           real(kind=dp), automatic :: sum0,sum1,sum2,sum3,sum4,sum5,sum6,sum7
           real(kind=dp), automatic :: sum8,sum9,sum10,sum11,sum12,sum13,sum14,sum15
           real(kind=dp), automatic :: f0,f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15
           real(kind=dp), automatic :: g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15
           real(kind=dp), automatic :: dt0,dt1,dt2,dt3,dt4,dt5,dt6,dt7
           real(kind=dp), automatic :: dt8,dt9,dt10,dt11,dt12,dt13,dt14,dt15
           real(kind=dp), automatic :: fdif,fsum,phi02,phi04,twopif,twopif0
           integer(kind=i4) :: i,m,m1,tin,nhtin
           tin    = htin*2
           nhtin  = -htin
           fdif   = f-fx
           fsum   = f+fx
           phi02  = 0.5_dp*phi0
           phi04  = 0.25_dp*phi0
           twopif = twopi*f
           twopif0=twopi*fx
           if(rho0/=0.5_dp) then
              call raster_flux_sinc_unroll_16x_r8(phif,htin,f,phi0)
              return
           else
              m = mod(tin,16)
              if(m /= 0) then
                 do i=1,m
                    t0      = real(i,kind=dp)
                    dt0     = dt+t0
                    a0      = pi*f*t0
                    f0      = phi02*t0
                    sinc0   = f0*sin(a0)/a0
                    cos0    = cos(twopif0*dt0)
                    b0      = pi*fdif*t0
                    sinc10  = cos0*sin(b0)/b0
                    c0      = pi*fsum*t0
                    g0      = phi04*t0
                    cos10   = cos(-twopif0*dt0)
                    sinc20  = cos10*sin(c0)/c0
                    sum0    = g0*(sinc0+sinc20)
                    phif(i) = (sinc0+sum0)*cos(-twopif*dt0)
                 end do
                 if(tin<16) return
              end if
              m1 = m+1
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(8)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=nhtin,m,16
                    t0        = real(i,kind=dp)
                    dt0       = dt+t0
                    a0        = pi*f*t0
                    f0        = phi02*t0
                    sinc0     = f0*sin(a0)/a0
                    cos0      = cos(twopif0*dt0)
                    b0        = pi*fdif*t0
                    sinc10    = cos0*sin(b0)/b0
                    c0        = pi*fsum*t0
                    g0        = phi04*t0
                    cos10     = cos(-twopif0*dt0)
                    sinc20    = cos10*sin(c0)/c0
                    sum0      = g0*(sinc0+sinc20)
                    phif(i)   = (sinc0+sum0)*cos(-twopif*dt0)
                    t1        = real(i-1,kind=dp)
                    dt1       = dt+t1
                    a1        = pi*f*t1
                    f1        = phi02*t1
                    sinc1     = f1*sin(a1)/a1
                    cos1      = cos(twopif0*dt1)
                    b1        = pi*fdif*t1
                    sinc11    = cos1*sin(b1)/b1
                    c1        = pi*fsum*t1
                    g1        = phi04*t1
                    cos11     = cos(-twopif0*dt1)
                    sinc21    = cos11*sin(c1)/c1
                    sum1      = g1*(sinc1+sinc21)
                    phif(i-1) = (sinc1+sum1)*cos(-twopif*dt1)
                    t2        = real(i-2,kind=dp)
                    dt2       = dt+t2
                    a2        = pi*f*t2
                    f2        = phi02*t2
                    sinc2     = f2*sin(a2)/a2
                    cos2      = cos(twopif0*dt2)
                    b2        = pi*fdif*t2
                    sinc12    = cos2*sin(b2)/b2
                    c2        = pi*fsum*t2
                    g2        = phi04*t2
                    cos12     = cos(-twopif0*dt2)
                    sinc22    = cos12*sin(c2)/c2
                    sum2      = g2*(sinc2+sinc22)
                    phif(i-2) = (sinc2+sum2)*cos(-twopif*dt2)
                    t3        = real(i-3,kind=dp)
                    dt3       = dt+t3
                    a3        = pi*f*t3
                    f3        = phi02*t3
                    sinc3     = f3*sin(a3)/a3
                    cos3      = cos(twopif0*dt3)
                    b3        = pi*fdif*t3
                    sinc13    = cos3*sin(b3)/b3
                    c3        = pi*fsum*t3
                    g3        = phi04*t3
                    cos13     = cos(-twopif0*dt3)
                    sinc23    = cos13*sin(c3)/c3
                    sum3      = g3*(sinc3+sinc23)
                    phif(i-3) = (sinc3+sum3)*cos(-twopif*dt3)
                    t4        = real(i-4,kind=dp)
                    dt4       = dt+t4
                    a4        = pi*f*t4
                    f4        = phi02*t4
                    sinc4     = f4*sin(a4)/a4
                    cos4      = cos(twopif0*dt4)
                    b4        = pi*fdif*t4
                    sinc14    = cos4*sin(b4)/b4
                    c4        = pi*fsum*t4
                    g4        = phi04*t4
                    cos14     = cos(-twopif0*dt4)
                    sinc24    = cos14*sin(c4)/c4
                    sum4      = g4*(sinc4+sinc24)
                    phif(i-4) = (sinc4+sum4)*cos(-twopif*dt4)
                    t5        = real(i-5,kind=dp)
                    dt5       = dt+t5
                    a5        = pi*f*t5
                    f5        = phi02*t5
                    sinc5     = f5*sin(a5)/a5
                    cos5      = cos(twopif0*dt5)
                    b5        = pi*fdif*t5
                    sinc15    = cos5*sin(b5)/b5
                    c5        = pi*fsum*t5
                    g5        = phi04*t5
                    cos15     = cos(-twopif0*dt5)
                    sinc25    = cos15*sin(c5)/c5
                    sum5      = g5*(sinc5+sinc25)
                    phif(i-5) = (sinc5+sum5)*cos(-twopif*dt5)
                    t6        = real(i-6,kind=dp)
                    dt6       = dt+t6
                    a6        = pi*f*t6
                    f6        = phi02*t6
                    sinc6     = f6*sin(a6)/a6
                    cos6      = cos(twopif0*dt6)
                    b6        = pi*fdif*t6
                    sinc16    = cos6*sin(b6)/b6
                    c6        = pi*fsum*t6
                    g6        = phi04*t6
                    cos16     = cos(-twopif0*dt6)
                    sinc26    = cos16*sin(c6)/c6
                    sum6      = g6*(sinc6+sinc26)
                    phif(i-6) = (sinc6+sum6)*cos(-twopif*dt6)
                    t7        = real(i-7,kind=dp)
                    dt7       = dt+t7
                    a7        = pi*f*t7
                    f7        = phi02*t7
                    sinc7     = f7*sin(a7)/a7
                    cos7      = cos(twopif0*dt7)
                    b7        = pi*fdif*t7
                    sinc17    = cos7*sin(b7)/b7
                    c7        = pi*fsum*t7
                    g7        = phi04*t7
                    cos17     = cos(-twopif0*dt7)
                    sinc27    = cos17*sin(c7)/c7
                    sum7      = g7*(sinc7+sinc27)
                    phif(i-7) = (sinc7+sum7)*cos(-twopif*dt7)
                    t8        = real(i-8,kind=dp)
                    dt8       = dt+t8
                    a8        = pi*f*t8
                    f8        = phi02*t8
                    sinc8     = f8*sin(a8)/a8
                    cos8      = cos(twopif0*dt8)
                    b8        = pi*fdif*t8
                    sinc18    = cos8*sin(b8)/b8
                    c8        = pi*fsum*t8
                    g8        = phi04*t8
                    cos18     = cos(-twopif0*dt8)
                    sinc28    = cos18*sin(c8)/c8
                    sum8      = g8*(sinc8+sinc28)
                    phif(i-8) = (sinc8+sum8)*cos(-twopif*dt8) 
                    t9        = real(i-9,kind=dp)
                    dt9       = dt+t9
                    a9        = pi*f*t9
                    f9        = phi02*t9
                    sinc9     = f9*sin(a9)/a9
                    cos9      = cos(twopif0*dt9)
                    b9        = pi*fdif*t9
                    sinc19    = cos1*sin(b9)/b9
                    c9        = pi*fsum*t9
                    g9        = phi04*t9
                    cos19     = cos(-twopif0*dt9)
                    sinc29    = cos19*sin(c9)/c9
                    sum9      = g9*(sinc9+sinc29)
                    phif(i-9) = (sinc9+sum9)*cos(-twopif*dt9)
                    t10       = real(i-10,kind=dp)
                    dt10      = dt+t10
                    a10       = pi*f*t10
                    f10       = phi02*t10
                    sinc10    = f10*sin(a10)/a10
                    cos10     = cos(twopif0*dt10)
                    b10       = pi*fdif*t10
                    sinc110   = cos10*sin(b10)/b10
                    c10       = pi*fsum*t10
                    g10       = phi04*t10
                    cos110    = cos(-twopif0*dt10)
                    sinc210   = cos110*sin(c10)/c10
                    sum10     = g10*(sinc10+sinc210)
                    phif(i-10)= (sinc10+sum10)*cos(-twopif*dt10)
                    t11       = real(i-11,kind=dp)
                    dt11      = dt+t11
                    a11       = pi*f*t11
                    f11       = phi02*t11
                    sinc11    = f11*sin(a11)/a11
                    cos11     = cos(twopif0*dt11)
                    b11       = pi*fdif*t11
                    sinc111   = cos11*sin(b11)/b11
                    c11       = pi*fsum*t11
                    g11       = phi04*t11
                    cos111    = cos(-twopif0*dt11)
                    sinc211   = cos111*sin(c11)/c11
                    sum11     = g11*(sinc11+sinc211)
                    phif(i-11)= (sinc11+sum11)*cos(-twopif*dt11)
                    t12       = real(i-12,kind=dp)
                    dt12      = dt+t12
                    a12       = pi*f*t12
                    f12       = phi02*t12
                    sinc12    = f12*sin(a12)/a12
                    cos12     = cos(twopif0*dt12)
                    b12       = pi*fdif*t12
                    sinc112   = cos12*sin(b12)/b12
                    c12       = pi*fsum*t12
                    g12       = phi04*t12
                    cos12     = cos(-twopif0*dt12)
                    sinc212   = cos12*sin(c12)/c12
                    sum12     = g12*(sinc12+sinc212)
                    phif(i-12)= (sinc12+sum12)*cos(-twopif*dt12)
                    t13       = real(i-13,kind=dp)
                    dt13      = dt+t13
                    a13       = pi*f*t13
                    f13       = phi02*t13
                    sinc13    = f13*sin(a13)/a13
                    cos13     = cos(twopif0*dt13)
                    b13       = pi*fdif*t13
                    sinc113   = cos13*sin(b13)/b13
                    c13       = pi*fsum*t13
                    g13       = phi04*t13
                    cos113    = cos(-twopif0*dt13)
                    sinc213   = cos13*sin(c13)/c13
                    sum13     = g13*(sinc13+sinc213)
                    phif(i-13)= (sinc13+sum13)*cos(-twopif*dt13)
                    t14       = real(i-14,kind=dp)
                    dt14      = dt+t14
                    a14       = pi*f*t14
                    f14       = phi02*t14
                    sinc14    = f14*sin(a14)/a14
                    cos14     = cos(twopif0*dt14)
                    b14       = pi*fdif*t14
                    sinc114   = cos14*sin(b14)/b14
                    c14       = pi*fsum*t14
                    g14       = phi04*t14
                    cos114    = cos(-twopif0*dt14)
                    sinc214   = cos14*sin(c14)/c14
                    sum14     = g14*(sinc14+sinc214)
                    phif(i-14)= (sinc14+sum14)*cos(-twopif*dt14)
                    t15       = real(i-15,kind=dp)
                    dt15      = dt+t15
                    a15       = pi*f*t15
                    f15       = phi02*t15
                    sinc15    = f15*sin(a15)/a15
                    cos15     = cos(twopif0*dt15)
                    b15       = pi*fdif*t15
                    sinc115   = cos15*sin(b15)/b15
                    c15       = pi*fsum*t15
                    g15       = phi04*t15
                    cos115    = cos(-twopif0*dt15)
                    sinc215   = cos15*sin(c15)/c15
                    sum15     = g15*(sinc15+sinc215)
                    phif(i-15)= (sinc15+sum15)*cos(-twopif*dt15)
              end do
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(8)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=m,htin,16
                    t0        = real(i,kind=dp)
                    dt0       = dt+t0
                    a0        = pi*f*t0
                    f0        = phi02*t0
                    sinc0     = f0*sin(a0)/a0
                    cos0      = cos(twopif0*dt0)
                    b0        = pi*fdif*t0
                    sinc10    = cos0*sin(b0)/b0
                    c0        = pi*fsum*t0
                    g0        = phi04*t0
                    cos10     = cos(-twopif0*dt0)
                    sinc20    = cos10*sin(c0)/c0
                    sum0      = g0*(sinc0+sinc20)
                    phif(i)   = (sinc0+sum0)*cos(-twopif*dt0)
                    t1        = real(i+1,kind=dp)
                    dt1       = dt+t1
                    a1        = pi*f*t1
                    f1        = phi02*t1
                    sinc1     = f1*sin(a1)/a1
                    cos1      = cos(twopif0*dt1)
                    b1        = pi*fdif*t1
                    sinc11    = cos1*sin(b1)/b1
                    c1        = pi*fsum*t1
                    g1        = phi04*t1
                    cos11     = cos(-twopif0*dt1)
                    sinc21    = cos11*sin(c1)/c1
                    sum1      = g1*(sinc1+sinc21)
                    phif(i+1) = (sinc1+sum1)*cos(-twopif*dt1)
                    t2        = real(i+2,kind=dp)
                    dt2       = dt+t2
                    a2        = pi*f*t2
                    f2        = phi02*t2
                    sinc2     = f2*sin(a2)/a2
                    cos2      = cos(twopif0*dt2)
                    b2        = pi*fdif*t2
                    sinc12    = cos2*sin(b2)/b2
                    c2        = pi*fsum*t2
                    g2        = phi04*t2
                    cos12     = cos(-twopif0*dt2)
                    sinc22    = cos12*sin(c2)/c2
                    sum2      = g2*(sinc2+sinc22)
                    phif(i+2) = (sinc2+sum2)*cos(-twopif*dt2)
                    t3        = real(i+3,kind=dp)
                    dt3       = dt+t3
                    a3        = pi*f*t3
                    f3        = phi02*t3
                    sinc3     = f3*sin(a3)/a3
                    cos3      = cos(twopif0*dt3)
                    b3        = pi*fdif*t3
                    sinc13    = cos3*sin(b3)/b3
                    c3        = pi*fsum*t3
                    g3        = phi04*t3
                    cos13     = cos(-twopif0*dt3)
                    sinc23    = cos13*sin(c3)/c3
                    sum3      = g3*(sinc3+sinc23)
                    phif(i+3) = (sinc3+sum3)*cos(-twopif*dt3)
                    t4        = real(i+4,kind=dp)
                    dt4       = dt+t4
                    a4        = pi*f*t4
                    f4        = phi02*t4
                    sinc4     = f4*sin(a4)/a4
                    cos4      = cos(twopif0*dt4)
                    b4        = pi*fdif*t4
                    sinc14    = cos4*sin(b4)/b4
                    c4        = pi*fsum*t4
                    g4        = phi04*t4
                    cos14     = cos(-twopif0*dt4)
                    sinc24    = cos14*sin(c4)/c4
                    sum4      = g4*(sinc4+sinc24)
                    phif(i+4) = (sinc4+sum4)*cos(-twopif*dt4)
                    t5        = real(i+5,kind=dp)
                    dt5       = dt+t5
                    a5        = pi*f*t5
                    f5        = phi02*t5
                    sinc5     = f5*sin(a5)/a5
                    cos5      = cos(twopif0*dt5)
                    b5        = pi*fdif*t5
                    sinc15    = cos5*sin(b5)/b5
                    c5        = pi*fsum*t5
                    g5        = phi04*t5
                    cos15     = cos(-twopif0*dt5)
                    sinc25    = cos15*sin(c5)/c5
                    sum5      = g5*(sinc5+sinc25)
                    phif(i+5) = (sinc5+sum5)*cos(-twopif*dt5)
                    t6        = real(i+6,kind=dp)
                    dt6       = dt+t6
                    a6        = pi*f*t6
                    f6        = phi02*t6
                    sinc6     = f6*sin(a6)/a6
                    cos6      = cos(twopif0*dt6)
                    b6        = pi*fdif*t6
                    sinc16    = cos6*sin(b6)/b6
                    c6        = pi*fsum*t6
                    g6        = phi04*t6
                    cos16     = cos(-twopif0*dt6)
                    sinc26    = cos16*sin(c6)/c6
                    sum6      = g6*(sinc6+sinc26)
                    phif(i+6) = (sinc6+sum6)*cos(-twopif*dt6)
                    t7        = real(i+7,kind=dp)
                    dt7       = dt+t7
                    a7        = pi*f*t7
                    f7        = phi02*t7
                    sinc7     = f7*sin(a7)/a7
                    cos7      = cos(twopif0*dt7)
                    b7        = pi*fdif*t7
                    sinc17    = cos7*sin(b7)/b7
                    c7        = pi*fsum*t7
                    g7        = phi04*t7
                    cos17     = cos(-twopif0*dt7)
                    sinc27    = cos17*sin(c7)/c7
                    sum7      = g7*(sinc7+sinc27)
                    phif(i+7) = (sinc7+sum7)*cos(-twopif*dt7)
                    t8        = real(i+8,kind=dp)
                    dt8       = dt+t8
                    a8        = pi*f*t8
                    f8        = phi02*t8
                    sinc8     = f8*sin(a8)/a8
                    cos8      = cos(twopif0*dt8)
                    b8        = pi*fdif*t8
                    sinc18    = cos8*sin(b8)/b8
                    c8        = pi*fsum*t8
                    g8        = phi04*t8
                    cos18     = cos(-twopif0*dt8)
                    sinc28    = cos18*sin(c8)/c8
                    sum8      = g8*(sinc8+sinc28)
                    phif(i+8) = (sinc8+sum8)*cos(-twopif*dt8) 
                    t9        = real(i+9,kind=dp)
                    dt9       = dt+t9
                    a9        = pi*f*t9
                    f9        = phi02*t9
                    sinc9     = f9*sin(a9)/a9
                    cos9      = cos(twopif0*dt9)
                    b9        = pi*fdif*t9
                    sinc19    = cos1*sin(b9)/b9
                    c9        = pi*fsum*t9
                    g9        = phi04*t9
                    cos19     = cos(-twopif0*dt9)
                    sinc29    = cos19*sin(c9)/c9
                    sum9      = g9*(sinc9+sinc29)
                    phif(i+9) = (sinc9+sum9)*cos(-twopif*dt9)
                    t10       = real(i+10,kind=dp)
                    dt10      = dt+t10
                    a10       = pi*f*t10
                    f10       = phi02*t10
                    sinc10    = f10*sin(a10)/a10
                    cos10     = cos(twopif0*dt10)
                    b10       = pi*fdif*t10
                    sinc110   = cos10*sin(b10)/b10
                    c10       = pi*fsum*t10
                    g10       = phi04*t10
                    cos110    = cos(-twopif0*dt10)
                    sinc210   = cos110*sin(c10)/c10
                    sum10     = g10*(sinc10+sinc210)
                    phif(i+10)= (sinc10+sum10)*cos(-twopif*dt10)
                    t11       = real(i+11,kind=dp)
                    dt11      = dt+t11
                    a11       = pi*f*t11
                    f11       = phi02*t11
                    sinc11    = f11*sin(a11)/a11
                    cos11     = cos(twopif0*dt11)
                    b11       = pi*fdif*t11
                    sinc111   = cos11*sin(b11)/b11
                    c11       = pi*fsum*t11
                    g11       = phi04*t11
                    cos111    = cos(-twopif0*dt11)
                    sinc211   = cos111*sin(c11)/c11
                    sum11     = g11*(sinc11+sinc211)
                    phif(i+11)= (sinc11+sum11)*cos(-twopif*dt11)
                    t12       = real(i+12,kind=dp)
                    dt12      = dt+t12
                    a12       = pi*f*t12
                    f12       = phi02*t12
                    sinc12    = f12*sin(a12)/a12
                    cos12     = cos(twopif0*dt12)
                    b12       = pi*fdif*t12
                    sinc112   = cos12*sin(b12)/b12
                    c12       = pi*fsum*t12
                    g12       = phi04*t12
                    cos12     = cos(-twopif0*dt12)
                    sinc212   = cos12*sin(c12)/c12
                    sum12     = g12*(sinc12+sinc212)
                    phif(i+12)= (sinc12+sum12)*cos(-twopif*dt12)
                    t13       = real(i+13,kind=dp)
                    dt13      = dt+t13
                    a13       = pi*f*t13
                    f13       = phi02*t13
                    sinc13    = f13*sin(a13)/a13
                    cos13     = cos(twopif0*dt13)
                    b13       = pi*fdif*t13
                    sinc113   = cos13*sin(b13)/b13
                    c13       = pi*fsum*t13
                    g13       = phi04*t13
                    cos113    = cos(-twopif0*dt13)
                    sinc213   = cos13*sin(c13)/c13
                    sum13     = g13*(sinc13+sinc213)
                    phif(i+13)= (sinc13+sum13)*cos(-twopif*dt13)
                    t14       = real(i+14,kind=dp)
                    dt14      = dt+t14
                    a14       = pi*f*t14
                    f14       = phi02*t14
                    sinc14    = f14*sin(a14)/a14
                    cos14     = cos(twopif0*dt14)
                    b14       = pi*fdif*t14
                    sinc114   = cos14*sin(b14)/b14
                    c14       = pi*fsum*t14
                    g14       = phi04*t14
                    cos114    = cos(-twopif0*dt14)
                    sinc214   = cos14*sin(c14)/c14
                    sum14     = g14*(sinc14+sinc214)
                    phif(i+14)= (sinc14+sum14)*cos(-twopif*dt14)
                    t15       = real(i+15,kind=dp)
                    dt15      = dt+t15
                    a15       = pi*f*t15
                    f15       = phi02*t15
                    sinc15    = f15*sin(a15)/a15
                    cos15     = cos(twopif0*dt15)
                    b15       = pi*fdif*t15
                    sinc115   = cos15*sin(b15)/b15
                    c15       = pi*fsum*t15
                    g15       = phi04*t15
                    cos115    = cos(-twopif0*dt15)
                    sinc215   = cos15*sin(c15)/c15
                    sum15     = g15*(sinc15+sinc215)
                    phif(i+15)= (sinc15+sum15)*cos(-twopif*dt15)
              end do
           end if
       end subroutine raster_mod_sinc_shifted_unroll_16x_r8 


       subroutine raster_mod_sinc_shifted_unroll_8x_r4(phif,htin,f,phi0,fx,rho0,dt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_mod_sinc_shifted_unroll_8x_r4
           !dir$ attributes forceinline ::   raster_mod_sinc_shifted_unroll_8x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_mod_sinc_shifted_unroll_8x_r4
           real(kind=sp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=sp),                         intent(in)  :: f
           real(kind=sp),                         intent(in)  :: phi0
           real(kind=sp),                         intent(in)  :: fx
           real(kind=sp),                         intent(in)  :: rho0
           real(kind=sp),                         intent(in)  :: dt
           real(kind=sp), parameter :: pi    = 3.14159265358979323846264338328_sp
           real(kind=sp), parameter :: twopi = 2.0_sp*pi
          
           real(kind=sp), automatic :: sinc0,sinc1,sinc2,sinc3,sinc4,sinc5,sinc6,sinc7
           real(kind=sp), automatic :: sinc10,sinc11,sinc12,sinc13,sinc14,sinc15,sinc16,sinc17
           real(kind=sp), automatic :: sinc20,sinc21,sinc22,sinc23,sinc24,sinc25,sinc26,sinc27
           real(kind=sp), automatic :: cos0,cos1,cos2,cos3,cos4,cos5,cos6,cos7
           real(kind=sp), automatic :: cos10,cos11,cos12,cos13,cos14,cos15,cos16,cos17
           real(kind=sp), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
           real(kind=sp), automatic :: b0,b1,b2,b3,b4,b5,b6,b7
           real(kind=sp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=sp), automatic :: c0,c1,c2,c3,c4,c5,c6,c7
           real(kind=sp), automatic :: sum0,sum1,sum2,sum3,sum4,sum5,sum6,sum7
           real(kind=sp), automatic :: f0,f1,f2,f3,f4,f5,f6,f7
           real(kind=sp), automatic :: g0,g1,g2,g3,g4,g5,g6,g7
           real(kind=sp), automatic :: dt0,dt1,dt2,dt3,dt4,dt5,dt6,dt7
           real(kind=sp), automatic :: fdif,fsum,phi02,phi04,twopif,twopif0
           integer(kind=i4) :: i,m,m1,tin,nhtin
           tin    = htin*2
           nhtin  = -htin
           fdif   = f-fx
           fsum   = f+fx
           phi02  = 0.5_sp*phi0
           phi04  = 0.25_sp*phi0
           twopif = twopi*f
           twopif0=twopi*fx
           if(rho0/=0.5_sp) then
              call raster_flux_sinc_unroll_8x_r4(phif,htin,f,phi0)
              return
           else
              m = mod(tin,8)
              if(m /= 0) then
                 do i=1,m
                    t0      = real(i,kind=sp)
                    dt0     = dt+t0
                    a0      = pi*f*t0
                    f0      = phi02*t0
                    sinc0   = f0*sin(a0)/a0
                    cos0    = cos(twopif0*dt0)
                    b0      = pi*fdif*t0
                    sinc10  = cos0*sin(b0)/b0
                    c0      = pi*fsum*t0
                    g0      = phi04*t0
                    cos10   = cos(-twopif0*dt0)
                    sinc20  = cos10*sin(c0)/c0
                    sum0    = g0*(sinc0+sinc20)
                    phif(i) = (sinc0+sum0)*cos(-twopif*dt0)
                 end do
                 if(tin<8) return
              end if
              m1 = m+1
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(4)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=nhtin,m,8
                    t0        = real(i,kind=sp)
                    dt0       = dt+t0
                    a0        = pi*f*t0
                    f0        = phi02*t0
                    sinc0     = f0*sin(a0)/a0
                    cos0      = cos(twopif0*dt0)
                    b0        = pi*fdif*t0
                    sinc10    = cos0*sin(b0)/b0
                    c0        = pi*fsum*t0
                    g0        = phi04*t0
                    cos10     = cos(-twopif0*dt0)
                    sinc20    = cos10*sin(c0)/c0
                    sum0      = g0*(sinc0+sinc20)
                    phif(i)   = (sinc0+sum0)*cos(-twopif*dt0)
                    t1        = real(i-1,kind=sp)
                    dt1       = dt+t1
                    a1        = pi*f*t1
                    f1        = phi02*t1
                    sinc1     = f1*sin(a1)/a1
                    cos1      = cos(twopif0*dt1)
                    b1        = pi*fdif*t1
                    sinc11    = cos1*sin(b1)/b1
                    c1        = pi*fsum*t1
                    g1        = phi04*t1
                    cos11     = cos(-twopif0*dt1)
                    sinc21    = cos11*sin(c1)/c1
                    sum1      = g1*(sinc1+sinc21)
                    phif(i-1) = (sinc1+sum1)*cos(-twopif*dt1)
                    t2        = real(i-2,kind=sp)
                    dt2       = dt+t2
                    a2        = pi*f*t2
                    f2        = phi02*t2
                    sinc2     = f2*sin(a2)/a2
                    cos2      = cos(twopif0*dt2)
                    b2        = pi*fdif*t2
                    sinc12    = cos2*sin(b2)/b2
                    c2        = pi*fsum*t2
                    g2        = phi04*t2
                    cos12     = cos(-twopif0*dt2)
                    sinc22    = cos12*sin(c2)/c2
                    sum2      = g2*(sinc2+sinc22)
                    phif(i-2) = (sinc2+sum2)*cos(-twopif*dt2)
                    t3        = real(i-3,kind=sp)
                    dt3       = dt+t3
                    a3        = pi*f*t3
                    f3        = phi02*t3
                    sinc3     = f3*sin(a3)/a3
                    cos3      = cos(twopif0*dt3)
                    b3        = pi*fdif*t3
                    sinc13    = cos3*sin(b3)/b3
                    c3        = pi*fsum*t3
                    g3        = phi04*t3
                    cos13     = cos(-twopif0*dt3)
                    sinc23    = cos13*sin(c3)/c3
                    sum3      = g3*(sinc3+sinc23)
                    phif(i-3) = (sinc3+sum3)*cos(-twopif*dt3)
                    t4        = real(i-4,kind=sp)
                    dt4       = dt+t4
                    a4        = pi*f*t4
                    f4        = phi02*t4
                    sinc4     = f4*sin(a4)/a4
                    cos4      = cos(twopif0*dt4)
                    b4        = pi*fdif*t4
                    sinc14    = cos4*sin(b4)/b4
                    c4        = pi*fsum*t4
                    g4        = phi04*t4
                    cos14     = cos(-twopif0*dt4)
                    sinc24    = cos14*sin(c4)/c4
                    sum4      = g4*(sinc4+sinc24)
                    phif(i-4) = (sinc4+sum4)*cos(-twopif*dt4)
                    t5        = real(i-5,kind=sp)
                    dt5       = dt+t5
                    a5        = pi*f*t5
                    f5        = phi02*t5
                    sinc5     = f5*sin(a5)/a5
                    cos5      = cos(twopif0*dt5)
                    b5        = pi*fdif*t5
                    sinc15    = cos5*sin(b5)/b5
                    c5        = pi*fsum*t5
                    g5        = phi04*t5
                    cos15     = cos(-twopif0*dt5)
                    sinc25    = cos15*sin(c5)/c5
                    sum5      = g5*(sinc5+sinc25)
                    phif(i-5) = (sinc5+sum5)*cos(-twopif*dt5)
                    t6        = real(i-6,kind=sp)
                    dt6       = dt+t6
                    a6        = pi*f*t6
                    f6        = phi02*t6
                    sinc6     = f6*sin(a6)/a6
                    cos6      = cos(twopif0*dt6)
                    b6        = pi*fdif*t6
                    sinc16    = cos6*sin(b6)/b6
                    c6        = pi*fsum*t6
                    g6        = phi04*t6
                    cos16     = cos(-twopif0*dt6)
                    sinc26    = cos16*sin(c6)/c6
                    sum6      = g6*(sinc6+sinc26)
                    phif(i-6) = (sinc6+sum6)*cos(-twopif*dt6)
                    t7        = real(i-7,kind=sp)
                    dt7       = dt+t7
                    a7        = pi*f*t7
                    f7        = phi02*t7
                    sinc7     = f7*sin(a7)/a7
                    cos7      = cos(twopif0*dt7)
                    b7        = pi*fdif*t7
                    sinc17    = cos7*sin(b7)/b7
                    c7        = pi*fsum*t7
                    g7        = phi04*t7
                    cos17     = cos(-twopif0*dt7)
                    sinc27    = cos17*sin(c7)/c7
                    sum7      = g7*(sinc7+sinc27)
                    phif(i-7) = (sinc7+sum7)*cos(-twopif*dt7)
              end do
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(4)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=m,htin,8
                    t0        = real(i,kind=sp)
                    dt0       = dt+t0
                    a0        = pi*f*t0
                    f0        = phi02*t0
                    sinc0     = f0*sin(a0)/a0
                    cos0      = cos(twopif0*dt0)
                    b0        = pi*fdif*t0
                    sinc10    = cos0*sin(b0)/b0
                    c0        = pi*fsum*t0
                    g0        = phi04*t0
                    cos10     = cos(-twopif0*dt0)
                    sinc20    = cos10*sin(c0)/c0
                    sum0      = g0*(sinc0+sinc20)
                    phif(i)   = (sinc0+sum0)*cos(-twopif*dt0)
                    t1        = real(i+1,kind=sp)
                    dt1       = dt+t1
                    a1        = pi*f*t1
                    f1        = phi02*t1
                    sinc1     = f1*sin(a1)/a1
                    cos1      = cos(twopif0*dt1)
                    b1        = pi*fdif*t1
                    sinc11    = cos1*sin(b1)/b1
                    c1        = pi*fsum*t1
                    g1        = phi04*t1
                    cos11     = cos(-twopif0*dt1)
                    sinc21    = cos11*sin(c1)/c1
                    sum1      = g1*(sinc1+sinc21)
                    phif(i+1) = (sinc1+sum1)*cos(-twopif*dt1)
                    t2        = real(i+2,kind=sp)
                    dt2       = dt+t2
                    a2        = pi*f*t2
                    f2        = phi02*t2
                    sinc2     = f2*sin(a2)/a2
                    cos2      = cos(twopif0*dt2)
                    b2        = pi*fdif*t2
                    sinc12    = cos2*sin(b2)/b2
                    c2        = pi*fsum*t2
                    g2        = phi04*t2
                    cos12     = cos(-twopif0*dt2)
                    sinc22    = cos12*sin(c2)/c2
                    sum2      = g2*(sinc2+sinc22)
                    phif(i+2) = (sinc2+sum2)*cos(-twopif*dt2)
                    t3        = real(i+3,kind=sp)
                    dt3       = dt+t3
                    a3        = pi*f*t3
                    f3        = phi02*t3
                    sinc3     = f3*sin(a3)/a3
                    cos3      = cos(twopif0*dt3)
                    b3        = pi*fdif*t3
                    sinc13    = cos3*sin(b3)/b3
                    c3        = pi*fsum*t3
                    g3        = phi04*t3
                    cos13     = cos(-twopif0*dt3)
                    sinc23    = cos13*sin(c3)/c3
                    sum3      = g3*(sinc3+sinc23)
                    phif(i+3) = (sinc3+sum3)*cos(-twopif*dt3)
                    t4        = real(i+4,kind=sp)
                    dt4       = dt+t4
                    a4        = pi*f*t4
                    f4        = phi02*t4
                    sinc4     = f4*sin(a4)/a4
                    cos4      = cos(twopif0*dt4)
                    b4        = pi*fdif*t4
                    sinc14    = cos4*sin(b4)/b4
                    c4        = pi*fsum*t4
                    g4        = phi04*t4
                    cos14     = cos(-twopif0*dt4)
                    sinc24    = cos14*sin(c4)/c4
                    sum4      = g4*(sinc4+sinc24)
                    phif(i+4) = (sinc4+sum4)*cos(-twopif*dt4)
                    t5        = real(i+5,kind=sp)
                    dt5       = dt+t5
                    a5        = pi*f*t5
                    f5        = phi02*t5
                    sinc5     = f5*sin(a5)/a5
                    cos5      = cos(twopif0*dt5)
                    b5        = pi*fdif*t5
                    sinc15    = cos5*sin(b5)/b5
                    c5        = pi*fsum*t5
                    g5        = phi04*t5
                    cos15     = cos(-twopif0*dt5)
                    sinc25    = cos15*sin(c5)/c5
                    sum5      = g5*(sinc5+sinc25)
                    phif(i+5) = (sinc5+sum5)*cos(-twopif*dt5)
                    t6        = real(i+6,kind=sp)
                    dt6       = dt+t6
                    a6        = pi*f*t6
                    f6        = phi02*t6
                    sinc6     = f6*sin(a6)/a6
                    cos6      = cos(twopif0*dt6)
                    b6        = pi*fdif*t6
                    sinc16    = cos6*sin(b6)/b6
                    c6        = pi*fsum*t6
                    g6        = phi04*t6
                    cos16     = cos(-twopif0*dt6)
                    sinc26    = cos16*sin(c6)/c6
                    sum6      = g6*(sinc6+sinc26)
                    phif(i+6) = (sinc6+sum6)*cos(-twopif*dt6)
                    t7        = real(i+7,kind=sp)
                    dt7       = dt+t7
                    a7        = pi*f*t7
                    f7        = phi02*t7
                    sinc7     = f7*sin(a7)/a7
                    cos7      = cos(twopif0*dt7)
                    b7        = pi*fdif*t7
                    sinc17    = cos7*sin(b7)/b7
                    c7        = pi*fsum*t7
                    g7        = phi04*t7
                    cos17     = cos(-twopif0*dt7)
                    sinc27    = cos17*sin(c7)/c7
                    sum7      = g7*(sinc7+sinc27)
                    phif(i+7) = (sinc7+sum7)*cos(-twopif*dt7)
               end do
           end if
       end subroutine raster_mod_sinc_shifted_unroll_8x_r4 


       subroutine raster_mod_sinc_shifted_unroll_8x_r8(phif,htin,f,phi0,fx,rho0,dt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_mod_sinc_shifted_unroll_8x_r8
           !dir$ attributes forceinline ::   raster_mod_sinc_shifted_unroll_8x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_mod_sinc_shifted_unroll_8x_r8
           real(kind=dp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=dp),                         intent(in)  :: f
           real(kind=dp),                         intent(in)  :: phi0
           real(kind=dp),                         intent(in)  :: fx
           real(kind=dp),                         intent(in)  :: rho0
           real(kind=dp),                         intent(in)  :: dt
           real(kind=dp), parameter :: pi    = 3.14159265358979323846264338328_dp
           real(kind=dp), parameter :: twopi = 2.0_dp*pi
           
           real(kind=dp), automatic :: sinc0,sinc1,sinc2,sinc3,sinc4,sinc5,sinc6,sinc7
           real(kind=dp), automatic :: sinc10,sinc11,sinc12,sinc13,sinc14,sinc15,sinc16,sinc17
           real(kind=dp), automatic :: sinc20,sinc21,sinc22,sinc23,sinc24,sinc25,sinc26,sinc27
           real(kind=dp), automatic :: cos0,cos1,cos2,cos3,cos4,cos5,cos6,cos7
           real(kind=dp), automatic :: cos10,cos11,cos12,cos13,cos14,cos15,cos16,cos17
           real(kind=dp), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
           real(kind=dp), automatic :: b0,b1,b2,b3,b4,b5,b6,b7
           real(kind=dp), automatic :: t0,t1,t2,t3,t4,t5,t6,t7
           real(kind=dp), automatic :: c0,c1,c2,c3,c4,c5,c6,c7
           real(kind=dp), automatic :: sum0,sum1,sum2,sum3,sum4,sum5,sum6,sum7
           real(kind=dp), automatic :: f0,f1,f2,f3,f4,f5,f6,f7
           real(kind=dp), automatic :: g0,g1,g2,g3,g4,g5,g6,g7
           real(kind=dp), automatic :: dt0,dt1,dt2,dt3,dt4,dt5,dt6,dt7
           real(kind=dp), automatic :: fdif,fsum,phi02,phi04,twopif,twopif0
           integer(kind=i4) :: i,m,m1,tin,nhtin
           tin    = htin*2
           nhtin  = -htin
           fdif   = f-fx
           fsum   = f+fx
           phi02  = 0.5_dp*phi0
           phi04  = 0.25_dp*phi0
           twopif = twopi*f
           twopif0=twopi*fx
           if(rho0/=0.5_dp) then
              call raster_flux_sinc_unroll_8x_r8(phif,htin,f,phi0)
              return
           else
              m = mod(tin,8)
              if(m /= 0) then
                 do i=1,m
                    t0      = real(i,kind=dp)
                    dt0     = dt+t0
                    a0      = pi*f*t0
                    f0      = phi02*t0
                    sinc0   = f0*sin(a0)/a0
                    cos0    = cos(twopif0*dt0)
                    b0      = pi*fdif*t0
                    sinc10  = cos0*sin(b0)/b0
                    c0      = pi*fsum*t0
                    g0      = phi04*t0
                    cos10   = cos(-twopif0*dt0)
                    sinc20  = cos10*sin(c0)/c0
                    sum0    = g0*(sinc0+sinc20)
                    phif(i) = (sinc0+sum0)*cos(-twopif*dt0)
                 end do
                 if(tin<8) return
              end if
              m1 = m+1
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(8)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=nhtin,m,8
                    t0        = real(i,kind=dp)
                    dt0       = dt+t0
                    a0        = pi*f*t0
                    f0        = phi02*t0
                    sinc0     = f0*sin(a0)/a0
                    cos0      = cos(twopif0*dt0)
                    b0        = pi*fdif*t0
                    sinc10    = cos0*sin(b0)/b0
                    c0        = pi*fsum*t0
                    g0        = phi04*t0
                    cos10     = cos(-twopif0*dt0)
                    sinc20    = cos10*sin(c0)/c0
                    sum0      = g0*(sinc0+sinc20)
                    phif(i)   = (sinc0+sum0)*cos(-twopif*dt0)
                    t1        = real(i-1,kind=dp)
                    dt1       = dt+t1
                    a1        = pi*f*t1
                    f1        = phi02*t1
                    sinc1     = f1*sin(a1)/a1
                    cos1      = cos(twopif0*dt1)
                    b1        = pi*fdif*t1
                    sinc11    = cos1*sin(b1)/b1
                    c1        = pi*fsum*t1
                    g1        = phi04*t1
                    cos11     = cos(-twopif0*dt1)
                    sinc21    = cos11*sin(c1)/c1
                    sum1      = g1*(sinc1+sinc21)
                    phif(i-1) = (sinc1+sum1)*cos(-twopif*dt1)
                    t2        = real(i-2,kind=dp)
                    dt2       = dt+t2
                    a2        = pi*f*t2
                    f2        = phi02*t2
                    sinc2     = f2*sin(a2)/a2
                    cos2      = cos(twopif0*dt2)
                    b2        = pi*fdif*t2
                    sinc12    = cos2*sin(b2)/b2
                    c2        = pi*fsum*t2
                    g2        = phi04*t2
                    cos12     = cos(-twopif0*dt2)
                    sinc22    = cos12*sin(c2)/c2
                    sum2      = g2*(sinc2+sinc22)
                    phif(i-2) = (sinc2+sum2)*cos(-twopif*dt2)
                    t3        = real(i-3,kind=dp)
                    dt3       = dt+t3
                    a3        = pi*f*t3
                    f3        = phi02*t3
                    sinc3     = f3*sin(a3)/a3
                    cos3      = cos(twopif0*dt3)
                    b3        = pi*fdif*t3
                    sinc13    = cos3*sin(b3)/b3
                    c3        = pi*fsum*t3
                    g3        = phi04*t3
                    cos13     = cos(-twopif0*dt3)
                    sinc23    = cos13*sin(c3)/c3
                    sum3      = g3*(sinc3+sinc23)
                    phif(i-3) = (sinc3+sum3)*cos(-twopif*dt3)
                    t4        = real(i-4,kind=dp)
                    dt4       = dt+t4
                    a4        = pi*f*t4
                    f4        = phi02*t4
                    sinc4     = f4*sin(a4)/a4
                    cos4      = cos(twopif0*dt4)
                    b4        = pi*fdif*t4
                    sinc14    = cos4*sin(b4)/b4
                    c4        = pi*fsum*t4
                    g4        = phi04*t4
                    cos14     = cos(-twopif0*dt4)
                    sinc24    = cos14*sin(c4)/c4
                    sum4      = g4*(sinc4+sinc24)
                    phif(i-4) = (sinc4+sum4)*cos(-twopif*dt4)
                    t5        = real(i-5,kind=dp)
                    dt5       = dt+t5
                    a5        = pi*f*t5
                    f5        = phi02*t5
                    sinc5     = f5*sin(a5)/a5
                    cos5      = cos(twopif0*dt5)
                    b5        = pi*fdif*t5
                    sinc15    = cos5*sin(b5)/b5
                    c5        = pi*fsum*t5
                    g5        = phi04*t5
                    cos15     = cos(-twopif0*dt5)
                    sinc25    = cos15*sin(c5)/c5
                    sum5      = g5*(sinc5+sinc25)
                    phif(i-5) = (sinc5+sum5)*cos(-twopif*dt5)
                    t6        = real(i-6,kind=dp)
                    dt6       = dt+t6
                    a6        = pi*f*t6
                    f6        = phi02*t6
                    sinc6     = f6*sin(a6)/a6
                    cos6      = cos(twopif0*dt6)
                    b6        = pi*fdif*t6
                    sinc16    = cos6*sin(b6)/b6
                    c6        = pi*fsum*t6
                    g6        = phi04*t6
                    cos16     = cos(-twopif0*dt6)
                    sinc26    = cos16*sin(c6)/c6
                    sum6      = g6*(sinc6+sinc26)
                    phif(i-6) = (sinc6+sum6)*cos(-twopif*dt6)
                    t7        = real(i-7,kind=dp)
                    dt7       = dt+t7
                    a7        = pi*f*t7
                    f7        = phi02*t7
                    sinc7     = f7*sin(a7)/a7
                    cos7      = cos(twopif0*dt7)
                    b7        = pi*fdif*t7
                    sinc17    = cos7*sin(b7)/b7
                    c7        = pi*fsum*t7
                    g7        = phi04*t7
                    cos17     = cos(-twopif0*dt7)
                    sinc27    = cos17*sin(c7)/c7
                    sum7      = g7*(sinc7+sinc27)
                    phif(i-7) = (sinc7+sum7)*cos(-twopif*dt7)
              end do
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(8)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=m,htin,8
                    t0        = real(i,kind=dp)
                    dt0       = dt+t0
                    a0        = pi*f*t0
                    f0        = phi02*t0
                    sinc0     = f0*sin(a0)/a0
                    cos0      = cos(twopif0*dt0)
                    b0        = pi*fdif*t0
                    sinc10    = cos0*sin(b0)/b0
                    c0        = pi*fsum*t0
                    g0        = phi04*t0
                    cos10     = cos(-twopif0*dt0)
                    sinc20    = cos10*sin(c0)/c0
                    sum0      = g0*(sinc0+sinc20)
                    phif(i)   = (sinc0+sum0)*cos(-twopif*dt0)
                    t1        = real(i+1,kind=dp)
                    dt1       = dt+t1
                    a1        = pi*f*t1
                    f1        = phi02*t1
                    sinc1     = f1*sin(a1)/a1
                    cos1      = cos(twopif0*dt1)
                    b1        = pi*fdif*t1
                    sinc11    = cos1*sin(b1)/b1
                    c1        = pi*fsum*t1
                    g1        = phi04*t1
                    cos11     = cos(-twopif0*dt1)
                    sinc21    = cos11*sin(c1)/c1
                    sum1      = g1*(sinc1+sinc21)
                    phif(i+1) = (sinc1+sum1)*cos(-twopif*dt1)
                    t2        = real(i+2,kind=dp)
                    dt2       = dt+t2
                    a2        = pi*f*t2
                    f2        = phi02*t2
                    sinc2     = f2*sin(a2)/a2
                    cos2      = cos(twopif0*dt2)
                    b2        = pi*fdif*t2
                    sinc12    = cos2*sin(b2)/b2
                    c2        = pi*fsum*t2
                    g2        = phi04*t2
                    cos12     = cos(-twopif0*dt2)
                    sinc22    = cos12*sin(c2)/c2
                    sum2      = g2*(sinc2+sinc22)
                    phif(i+2) = (sinc2+sum2)*cos(-twopif*dt2)
                    t3        = real(i+3,kind=dp)
                    dt3       = dt+t3
                    a3        = pi*f*t3
                    f3        = phi02*t3
                    sinc3     = f3*sin(a3)/a3
                    cos3      = cos(twopif0*dt3)
                    b3        = pi*fdif*t3
                    sinc13    = cos3*sin(b3)/b3
                    c3        = pi*fsum*t3
                    g3        = phi04*t3
                    cos13     = cos(-twopif0*dt3)
                    sinc23    = cos13*sin(c3)/c3
                    sum3      = g3*(sinc3+sinc23)
                    phif(i+3) = (sinc3+sum3)*cos(-twopif*dt3)
                    t4        = real(i+4,kind=dp)
                    dt4       = dt+t4
                    a4        = pi*f*t4
                    f4        = phi02*t4
                    sinc4     = f4*sin(a4)/a4
                    cos4      = cos(twopif0*dt4)
                    b4        = pi*fdif*t4
                    sinc14    = cos4*sin(b4)/b4
                    c4        = pi*fsum*t4
                    g4        = phi04*t4
                    cos14     = cos(-twopif0*dt4)
                    sinc24    = cos14*sin(c4)/c4
                    sum4      = g4*(sinc4+sinc24)
                    phif(i+4) = (sinc4+sum4)*cos(-twopif*dt4)
                    t5        = real(i+5,kind=dp)
                    dt5       = dt+t5
                    a5        = pi*f*t5
                    f5        = phi02*t5
                    sinc5     = f5*sin(a5)/a5
                    cos5      = cos(twopif0*dt5)
                    b5        = pi*fdif*t5
                    sinc15    = cos5*sin(b5)/b5
                    c5        = pi*fsum*t5
                    g5        = phi04*t5
                    cos15     = cos(-twopif0*dt5)
                    sinc25    = cos15*sin(c5)/c5
                    sum5      = g5*(sinc5+sinc25)
                    phif(i+5) = (sinc5+sum5)*cos(-twopif*dt5)
                    t6        = real(i+6,kind=dp)
                    dt6       = dt+t6
                    a6        = pi*f*t6
                    f6        = phi02*t6
                    sinc6     = f6*sin(a6)/a6
                    cos6      = cos(twopif0*dt6)
                    b6        = pi*fdif*t6
                    sinc16    = cos6*sin(b6)/b6
                    c6        = pi*fsum*t6
                    g6        = phi04*t6
                    cos16     = cos(-twopif0*dt6)
                    sinc26    = cos16*sin(c6)/c6
                    sum6      = g6*(sinc6+sinc26)
                    phif(i+6) = (sinc6+sum6)*cos(-twopif*dt6)
                    t7        = real(i+7,kind=dp)
                    dt7       = dt+t7
                    a7        = pi*f*t7
                    f7        = phi02*t7
                    sinc7     = f7*sin(a7)/a7
                    cos7      = cos(twopif0*dt7)
                    b7        = pi*fdif*t7
                    sinc17    = cos7*sin(b7)/b7
                    c7        = pi*fsum*t7
                    g7        = phi04*t7
                    cos17     = cos(-twopif0*dt7)
                    sinc27    = cos17*sin(c7)/c7
                    sum7      = g7*(sinc7+sinc27)
                    phif(i+7) = (sinc7+sum7)*cos(-twopif*dt7)
               end do
           end if
       end subroutine raster_mod_sinc_shifted_unroll_8x_r8 


       subroutine raster_mod_sinc_shifted_unroll_4x_r4(phif,htin,f,phi0,fx,rho0,dt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_mod_sinc_shifted_unroll_4x_r4
           !dir$ attributes forceinline ::   raster_mod_sinc_shifted_unroll_4x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_mod_sinc_shifted_unroll_4x_r4
           real(kind=sp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=sp),                         intent(in)  :: f
           real(kind=sp),                         intent(in)  :: phi0
           real(kind=sp),                         intent(in)  :: fx
           real(kind=sp),                         intent(in)  :: rho0
           real(kind=sp),                         intent(in)  :: dt
           real(kind=sp), parameter :: pi    = 3.14159265358979323846264338328_sp
           real(kind=sp), parameter :: twopi = 2.0_sp*pi
          
           real(kind=sp), automatic :: sinc0,sinc1,sinc2,sinc3
           real(kind=sp), automatic :: sinc10,sinc11,sinc12,sinc13
           real(kind=sp), automatic :: sinc20,sinc21,sinc22,sinc23
           real(kind=sp), automatic :: cos0,cos1,cos2,cos3
           real(kind=sp), automatic :: cos10,cos11,cos12,cos13
           real(kind=sp), automatic :: a0,a1,a2,a3
           real(kind=sp), automatic :: b0,b1,b2,b3
           real(kind=sp), automatic :: t0,t1,t2,t3
           real(kind=sp), automatic :: c0,c1,c2,c3
           real(kind=sp), automatic :: sum0,sum1,sum2,sum3
           real(kind=sp), automatic :: f0,f1,f2,f3
           real(kind=sp), automatic :: g0,g1,g2,g3
           real(kind=sp), automatic :: dt0,dt1,dt2,dt3
           real(kind=sp), automatic :: fdif,fsum,phi02,phi04,twopif,twopif0
           integer(kind=i4) :: i,m,m1,tin,nhtin
           tin    = htin*2
           nhtin  = -htin
           fdif   = f-fx
           fsum   = f+fx
           phi02  = 0.5_sp*phi0
           phi04  = 0.25_sp*phi0
           twopif = twopi*f
           twopif0=twopi*fx
           if(rho0/=0.5_sp) then
              call raster_flux_sinc_unroll_4x_r4(phif,htin,f,phi0)
              return
           else
              m = mod(tin,4)
              if(m /= 0) then
                 do i=1,m
                    t0      = real(i,kind=sp)
                    dt0     = dt+t0
                    a0      = pi*f*t0
                    f0      = phi02*t0
                    sinc0   = f0*sin(a0)/a0
                    cos0    = cos(twopif0*dt0)
                    b0      = pi*fdif*t0
                    sinc10  = cos0*sin(b0)/b0
                    c0      = pi*fsum*t0
                    g0      = phi04*t0
                    cos10   = cos(-twopif0*dt0)
                    sinc20  = cos10*sin(c0)/c0
                    sum0    = g0*(sinc0+sinc20)
                    phif(i) = (sinc0+sum0)*cos(-twopif*dt0)
                 end do
                 if(tin<4) return
              end if
              m1 = m+1
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(4)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=nhtin,m,4
                    t0        = real(i,kind=sp)
                    dt0       = dt+t0
                    a0        = pi*f*t0
                    f0        = phi02*t0
                    sinc0     = f0*sin(a0)/a0
                    cos0      = cos(twopif0*dt0)
                    b0        = pi*fdif*t0
                    sinc10    = cos0*sin(b0)/b0
                    c0        = pi*fsum*t0
                    g0        = phi04*t0
                    cos10     = cos(-twopif0*dt0)
                    sinc20    = cos10*sin(c0)/c0
                    sum0      = g0*(sinc0+sinc20)
                    phif(i)   = (sinc0+sum0)*cos(-twopif*dt0)
                    t1        = real(i-1,kind=sp)
                    dt1       = dt+t1
                    a1        = pi*f*t1
                    f1        = phi02*t1
                    sinc1     = f1*sin(a1)/a1
                    cos1      = cos(twopif0*dt1)
                    b1        = pi*fdif*t1
                    sinc11    = cos1*sin(b1)/b1
                    c1        = pi*fsum*t1
                    g1        = phi04*t1
                    cos11     = cos(-twopif0*dt1)
                    sinc21    = cos11*sin(c1)/c1
                    sum1      = g1*(sinc1+sinc21)
                    phif(i-1) = (sinc1+sum1)*cos(-twopif*dt1)
                    t2        = real(i-2,kind=sp)
                    dt2       = dt+t2
                    a2        = pi*f*t2
                    f2        = phi02*t2
                    sinc2     = f2*sin(a2)/a2
                    cos2      = cos(twopif0*dt2)
                    b2        = pi*fdif*t2
                    sinc12    = cos2*sin(b2)/b2
                    c2        = pi*fsum*t2
                    g2        = phi04*t2
                    cos12     = cos(-twopif0*dt2)
                    sinc22    = cos12*sin(c2)/c2
                    sum2      = g2*(sinc2+sinc22)
                    phif(i-2) = (sinc2+sum2)*cos(-twopif*dt2)
                    t3        = real(i-3,kind=sp)
                    dt3       = dt+t3
                    a3        = pi*f*t3
                    f3        = phi02*t3
                    sinc3     = f3*sin(a3)/a3
                    cos3      = cos(twopif0*dt3)
                    b3        = pi*fdif*t3
                    sinc13    = cos3*sin(b3)/b3
                    c3        = pi*fsum*t3
                    g3        = phi04*t3
                    cos13     = cos(-twopif0*dt3)
                    sinc23    = cos13*sin(c3)/c3
                    sum3      = g3*(sinc3+sinc23)
                    phif(i-3) = (sinc3+sum3)*cos(-twopif*dt3)
              end do
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(4)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=m,htin,4
                    t0        = real(i,kind=sp)
                    dt0       = dt+t0
                    a0        = pi*f*t0
                    f0        = phi02*t0
                    sinc0     = f0*sin(a0)/a0
                    cos0      = cos(twopif0*dt0)
                    b0        = pi*fdif*t0
                    sinc10    = cos0*sin(b0)/b0
                    c0        = pi*fsum*t0
                    g0        = phi04*t0
                    cos10     = cos(-twopif0*dt0)
                    sinc20    = cos10*sin(c0)/c0
                    sum0      = g0*(sinc0+sinc20)
                    phif(i)   = (sinc0+sum0)*cos(-twopif*dt0)
                    t1        = real(i+1,kind=sp)
                    dt1       = dt+t1
                    a1        = pi*f*t1
                    f1        = phi02*t1
                    sinc1     = f1*sin(a1)/a1
                    cos1      = cos(twopif0*dt1)
                    b1        = pi*fdif*t1
                    sinc11    = cos1*sin(b1)/b1
                    c1        = pi*fsum*t1
                    g1        = phi04*t1
                    cos11     = cos(-twopif0*dt1)
                    sinc21    = cos11*sin(c1)/c1
                    sum1      = g1*(sinc1+sinc21)
                    phif(i+1) = (sinc1+sum1)*cos(-twopif*dt1)
                    t2        = real(i+2,kind=sp)
                    dt2       = dt+t2
                    a2        = pi*f*t2
                    f2        = phi02*t2
                    sinc2     = f2*sin(a2)/a2
                    cos2      = cos(twopif0*dt2)
                    b2        = pi*fdif*t2
                    sinc12    = cos2*sin(b2)/b2
                    c2        = pi*fsum*t2
                    g2        = phi04*t2
                    cos12     = cos(-twopif0*dt2)
                    sinc22    = cos12*sin(c2)/c2
                    sum2      = g2*(sinc2+sinc22)
                    phif(i+2) = (sinc2+sum2)*cos(-twopif*dt2)
                    t3        = real(i+3,kind=sp)
                    dt3       = dt+t3
                    a3        = pi*f*t3
                    f3        = phi02*t3
                    sinc3     = f3*sin(a3)/a3
                    cos3      = cos(twopif0*dt3)
                    b3        = pi*fdif*t3
                    sinc13    = cos3*sin(b3)/b3
                    c3        = pi*fsum*t3
                    g3        = phi04*t3
                    cos13     = cos(-twopif0*dt3)
                    sinc23    = cos13*sin(c3)/c3
                    sum3      = g3*(sinc3+sinc23)
                    phif(i+3) = (sinc3+sum3)*cos(-twopif*dt3)
                   
               end do
           end if
       end subroutine raster_mod_sinc_shifted_unroll_4x_r4 


       subroutine raster_mod_sinc_shifted_unroll_4x_r8(phif,htin,f,phi0,fx,rho0,dt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_mod_sinc_shifted_unroll_4x_r8
           !dir$ attributes forceinline ::   raster_mod_sinc_shifted_unroll_4x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_mod_sinc_shifted_unroll_4x_r8
           real(kind=dp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=dp),                         intent(in)  :: f
           real(kind=dp),                         intent(in)  :: phi0
           real(kind=dp),                         intent(in)  :: fx
           real(kind=dp),                         intent(in)  :: rho0
           real(kind=dp),                         intent(in)  :: dt
           real(kind=dp), parameter :: pi    = 3.14159265358979323846264338328_dp
           real(kind=dp), parameter :: twopi = 2.0_dp*pi
           
           real(kind=dp), automatic :: sinc0,sinc1,sinc2,sinc3
           real(kind=dp), automatic :: sinc10,sinc11,sinc12,sinc13
           real(kind=dp), automatic :: sinc20,sinc21,sinc22,sinc23
           real(kind=dp), automatic :: cos0,cos1,cos2,cos3,
           real(kind=dp), automatic :: cos10,cos11,cos12,cos13
           real(kind=dp), automatic :: a0,a1,a2,a3
           real(kind=dp), automatic :: b0,b1,b2,b3
           real(kind=dp), automatic :: t0,t1,t2,t3
           real(kind=dp), automatic :: c0,c1,c2,c3
           real(kind=dp), automatic :: sum0,sum1,sum2,sum3
           real(kind=dp), automatic :: f0,f1,f2,f3
           real(kind=dp), automatic :: g0,g1,g2,g3
           real(kind=dp), automatic :: dt0,dt1,dt2,dt3
           real(kind=dp), automatic :: fdif,fsum,phi02,phi04,twopif,twopif0
           integer(kind=i4) :: i,m,m1,tin,nhtin
           tin    = htin*2
           nhtin  = -htin
           fdif   = f-fx
           fsum   = f+fx
           phi02  = 0.5_dp*phi0
           phi04  = 0.25_dp*phi0
           twopif = twopi*f
           twopif0=twopi*fx
           if(rho0/=0.5_dp) then
              call raster_flux_sinc_unroll_4x_r8(phif,htin,f,phi0)
              return
           else
              m = mod(tin,4)
              if(m /= 0) then
                 do i=1,m
                    t0      = real(i,kind=dp)
                    dt0     = dt+t0
                    a0      = pi*f*t0
                    f0      = phi02*t0
                    sinc0   = f0*sin(a0)/a0
                    cos0    = cos(twopif0*dt0)
                    b0      = pi*fdif*t0
                    sinc10  = cos0*sin(b0)/b0
                    c0      = pi*fsum*t0
                    g0      = phi04*t0
                    cos10   = cos(-twopif0*dt0)
                    sinc20  = cos10*sin(c0)/c0
                    sum0    = g0*(sinc0+sinc20)
                    phif(i) = (sinc0+sum0)*cos(-twopif*dt0)
                 end do
                 if(tin<4) return
              end if
              m1 = m+1
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(8)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=nhtin,m,4
                    t0        = real(i,kind=dp)
                    dt0       = dt+t0
                    a0        = pi*f*t0
                    f0        = phi02*t0
                    sinc0     = f0*sin(a0)/a0
                    cos0      = cos(twopif0*dt0)
                    b0        = pi*fdif*t0
                    sinc10    = cos0*sin(b0)/b0
                    c0        = pi*fsum*t0
                    g0        = phi04*t0
                    cos10     = cos(-twopif0*dt0)
                    sinc20    = cos10*sin(c0)/c0
                    sum0      = g0*(sinc0+sinc20)
                    phif(i)   = (sinc0+sum0)*cos(-twopif*dt0)
                    t1        = real(i-1,kind=dp)
                    dt1       = dt+t1
                    a1        = pi*f*t1
                    f1        = phi02*t1
                    sinc1     = f1*sin(a1)/a1
                    cos1      = cos(twopif0*dt1)
                    b1        = pi*fdif*t1
                    sinc11    = cos1*sin(b1)/b1
                    c1        = pi*fsum*t1
                    g1        = phi04*t1
                    cos11     = cos(-twopif0*dt1)
                    sinc21    = cos11*sin(c1)/c1
                    sum1      = g1*(sinc1+sinc21)
                    phif(i-1) = (sinc1+sum1)*cos(-twopif*dt1)
                    t2        = real(i-2,kind=dp)
                    dt2       = dt+t2
                    a2        = pi*f*t2
                    f2        = phi02*t2
                    sinc2     = f2*sin(a2)/a2
                    cos2      = cos(twopif0*dt2)
                    b2        = pi*fdif*t2
                    sinc12    = cos2*sin(b2)/b2
                    c2        = pi*fsum*t2
                    g2        = phi04*t2
                    cos12     = cos(-twopif0*dt2)
                    sinc22    = cos12*sin(c2)/c2
                    sum2      = g2*(sinc2+sinc22)
                    phif(i-2) = (sinc2+sum2)*cos(-twopif*dt2)
                    t3        = real(i-3,kind=dp)
                    dt3       = dt+t3
                    a3        = pi*f*t3
                    f3        = phi02*t3
                    sinc3     = f3*sin(a3)/a3
                    cos3      = cos(twopif0*dt3)
                    b3        = pi*fdif*t3
                    sinc13    = cos3*sin(b3)/b3
                    c3        = pi*fsum*t3
                    g3        = phi04*t3
                    cos13     = cos(-twopif0*dt3)
                    sinc23    = cos13*sin(c3)/c3
                    sum3      = g3*(sinc3+sinc23)
                    phif(i-3) = (sinc3+sum3)*cos(-twopif*dt3)
                   
              end do
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(8)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=m,htin,4
                    t0        = real(i,kind=dp)
                    dt0       = dt+t0
                    a0        = pi*f*t0
                    f0        = phi02*t0
                    sinc0     = f0*sin(a0)/a0
                    cos0      = cos(twopif0*dt0)
                    b0        = pi*fdif*t0
                    sinc10    = cos0*sin(b0)/b0
                    c0        = pi*fsum*t0
                    g0        = phi04*t0
                    cos10     = cos(-twopif0*dt0)
                    sinc20    = cos10*sin(c0)/c0
                    sum0      = g0*(sinc0+sinc20)
                    phif(i)   = (sinc0+sum0)*cos(-twopif*dt0)
                    t1        = real(i+1,kind=dp)
                    dt1       = dt+t1
                    a1        = pi*f*t1
                    f1        = phi02*t1
                    sinc1     = f1*sin(a1)/a1
                    cos1      = cos(twopif0*dt1)
                    b1        = pi*fdif*t1
                    sinc11    = cos1*sin(b1)/b1
                    c1        = pi*fsum*t1
                    g1        = phi04*t1
                    cos11     = cos(-twopif0*dt1)
                    sinc21    = cos11*sin(c1)/c1
                    sum1      = g1*(sinc1+sinc21)
                    phif(i+1) = (sinc1+sum1)*cos(-twopif*dt1)
                    t2        = real(i+2,kind=dp)
                    dt2       = dt+t2
                    a2        = pi*f*t2
                    f2        = phi02*t2
                    sinc2     = f2*sin(a2)/a2
                    cos2      = cos(twopif0*dt2)
                    b2        = pi*fdif*t2
                    sinc12    = cos2*sin(b2)/b2
                    c2        = pi*fsum*t2
                    g2        = phi04*t2
                    cos12     = cos(-twopif0*dt2)
                    sinc22    = cos12*sin(c2)/c2
                    sum2      = g2*(sinc2+sinc22)
                    phif(i+2) = (sinc2+sum2)*cos(-twopif*dt2)
                    t3        = real(i+3,kind=dp)
                    dt3       = dt+t3
                    a3        = pi*f*t3
                    f3        = phi02*t3
                    sinc3     = f3*sin(a3)/a3
                    cos3      = cos(twopif0*dt3)
                    b3        = pi*fdif*t3
                    sinc13    = cos3*sin(b3)/b3
                    c3        = pi*fsum*t3
                    g3        = phi04*t3
                    cos13     = cos(-twopif0*dt3)
                    sinc23    = cos13*sin(c3)/c3
                    sum3      = g3*(sinc3+sinc23)
                    phif(i+3) = (sinc3+sum3)*cos(-twopif*dt3)
                   
               end do
           end if
       end subroutine raster_mod_sinc_shifted_unroll_4x_r8 


       subroutine raster_mod_sinc_shifted_unroll_2x_r4(phif,htin,f,phi0,fx,rho0,dt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_mod_sinc_shifted_unroll_2x_r4
           !dir$ attributes forceinline ::   raster_mod_sinc_shifted_unroll_2x_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_mod_sinc_shifted_unroll_2x_r4
           real(kind=sp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=sp),                         intent(in)  :: f
           real(kind=sp),                         intent(in)  :: phi0
           real(kind=sp),                         intent(in)  :: fx
           real(kind=sp),                         intent(in)  :: rho0
           real(kind=sp),                         intent(in)  :: dt
           real(kind=sp), parameter :: pi    = 3.14159265358979323846264338328_sp
           real(kind=sp), parameter :: twopi = 2.0_sp*pi
          
           real(kind=sp), automatic :: sinc0,sinc1
           real(kind=sp), automatic :: sinc10,sinc11
           real(kind=sp), automatic :: sinc20,sinc21
           real(kind=sp), automatic :: cos0,cos1
           real(kind=sp), automatic :: cos10,cos11
           real(kind=sp), automatic :: a0,a1
           real(kind=sp), automatic :: b0,b1
           real(kind=sp), automatic :: t0,t1
           real(kind=sp), automatic :: c0,c1
           real(kind=sp), automatic :: sum0,sum1
           real(kind=sp), automatic :: f0,f1
           real(kind=sp), automatic :: g0,g1
           real(kind=sp), automatic :: dt0,dt1
           real(kind=sp), automatic :: fdif,fsum,phi02,phi04,twopif,twopif0
           integer(kind=i4) :: i,m,m1,tin,nhtin
           tin    = htin*2
           nhtin  = -htin
           fdif   = f-fx
           fsum   = f+fx
           phi02  = 0.5_sp*phi0
           phi04  = 0.25_sp*phi0
           twopif = twopi*f
           twopif0=twopi*fx
           if(rho0/=0.5_sp) then
              call raster_flux_sinc_unroll_2x_r4(phif,htin,f,phi0)
              return
           else
              m = mod(tin,2)
              if(m /= 0) then
                 do i=1,m
                    t0      = real(i,kind=sp)
                    dt0     = dt+t0
                    a0      = pi*f*t0
                    f0      = phi02*t0
                    sinc0   = f0*sin(a0)/a0
                    cos0    = cos(twopif0*dt0)
                    b0      = pi*fdif*t0
                    sinc10  = cos0*sin(b0)/b0
                    c0      = pi*fsum*t0
                    g0      = phi04*t0
                    cos10   = cos(-twopif0*dt0)
                    sinc20  = cos10*sin(c0)/c0
                    sum0    = g0*(sinc0+sinc20)
                    phif(i) = (sinc0+sum0)*cos(-twopif*dt0)
                 end do
                 if(tin<2) return
              end if
              m1 = m+1
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(4)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=nhtin,m,2
                    t0        = real(i,kind=sp)
                    dt0       = dt+t0
                    a0        = pi*f*t0
                    f0        = phi02*t0
                    sinc0     = f0*sin(a0)/a0
                    cos0      = cos(twopif0*dt0)
                    b0        = pi*fdif*t0
                    sinc10    = cos0*sin(b0)/b0
                    c0        = pi*fsum*t0
                    g0        = phi04*t0
                    cos10     = cos(-twopif0*dt0)
                    sinc20    = cos10*sin(c0)/c0
                    sum0      = g0*(sinc0+sinc20)
                    phif(i)   = (sinc0+sum0)*cos(-twopif*dt0)
                    t1        = real(i-1,kind=sp)
                    dt1       = dt+t1
                    a1        = pi*f*t1
                    f1        = phi02*t1
                    sinc1     = f1*sin(a1)/a1
                    cos1      = cos(twopif0*dt1)
                    b1        = pi*fdif*t1
                    sinc11    = cos1*sin(b1)/b1
                    c1        = pi*fsum*t1
                    g1        = phi04*t1
                    cos11     = cos(-twopif0*dt1)
                    sinc21    = cos11*sin(c1)/c1
                    sum1      = g1*(sinc1+sinc21)
                    phif(i-1) = (sinc1+sum1)*cos(-twopif*dt1)
                   
              end do
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(4)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=m,htin,2
                    t0        = real(i,kind=sp)
                    dt0       = dt+t0
                    a0        = pi*f*t0
                    f0        = phi02*t0
                    sinc0     = f0*sin(a0)/a0
                    cos0      = cos(twopif0*dt0)
                    b0        = pi*fdif*t0
                    sinc10    = cos0*sin(b0)/b0
                    c0        = pi*fsum*t0
                    g0        = phi04*t0
                    cos10     = cos(-twopif0*dt0)
                    sinc20    = cos10*sin(c0)/c0
                    sum0      = g0*(sinc0+sinc20)
                    phif(i)   = (sinc0+sum0)*cos(-twopif*dt0)
                    t1        = real(i+1,kind=sp)
                    dt1       = dt+t1
                    a1        = pi*f*t1
                    f1        = phi02*t1
                    sinc1     = f1*sin(a1)/a1
                    cos1      = cos(twopif0*dt1)
                    b1        = pi*fdif*t1
                    sinc11    = cos1*sin(b1)/b1
                    c1        = pi*fsum*t1
                    g1        = phi04*t1
                    cos11     = cos(-twopif0*dt1)
                    sinc21    = cos11*sin(c1)/c1
                    sum1      = g1*(sinc1+sinc21)
                    phif(i+1) = (sinc1+sum1)*cos(-twopif*dt1)
                   
                   
               end do
           end if
       end subroutine raster_mod_sinc_shifted_unroll_2x_r4 


       subroutine raster_mod_sinc_shifted_unroll_2x_r8(phif,htin,f,phi0,fx,rho0,dt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_mod_sinc_shifted_unroll_2x_r8
           !dir$ attributes forceinline ::   raster_mod_sinc_shifted_unroll_2x_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_mod_sinc_shifted_unroll_2x_r8
           real(kind=dp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=dp),                         intent(in)  :: f
           real(kind=dp),                         intent(in)  :: phi0
           real(kind=dp),                         intent(in)  :: fx
           real(kind=dp),                         intent(in)  :: rho0
           real(kind=dp),                         intent(in)  :: dt
           real(kind=dp), parameter :: pi    = 3.14159265358979323846264338328_dp
           real(kind=dp), parameter :: twopi = 2.0_dp*pi
           
           real(kind=dp), automatic :: sinc0,sinc1
           real(kind=dp), automatic :: sinc10,sinc11
           real(kind=dp), automatic :: sinc20,sinc21
           real(kind=dp), automatic :: cos0,cos1
           real(kind=dp), automatic :: cos10,cos11
           real(kind=dp), automatic :: a0,a1
           real(kind=dp), automatic :: b0,b1
           real(kind=dp), automatic :: t0,t1
           real(kind=dp), automatic :: c0,c1
           real(kind=dp), automatic :: sum0,sum1
           real(kind=dp), automatic :: f0,f1
           real(kind=dp), automatic :: g0,g1
           real(kind=dp), automatic :: dt0,dt1
           real(kind=dp), automatic :: fdif,fsum,phi02,phi04,twopif,twopif0
           integer(kind=i4) :: i,m,m1,tin,nhtin
           tin    = htin*2
           nhtin  = -htin
           fdif   = f-fx
           fsum   = f+fx
           phi02  = 0.5_dp*phi0
           phi04  = 0.25_dp*phi0
           twopif = twopi*f
           twopif0=twopi*fx
           if(rho0/=0.5_dp) then
              call raster_flux_sinc_unroll_2x_r8(phif,htin,f,phi0)
              return
           else
              m = mod(tin,2)
              if(m /= 0) then
                 do i=1,m
                    t0      = real(i,kind=dp)
                    dt0     = dt+t0
                    a0      = pi*f*t0
                    f0      = phi02*t0
                    sinc0   = f0*sin(a0)/a0
                    cos0    = cos(twopif0*dt0)
                    b0      = pi*fdif*t0
                    sinc10  = cos0*sin(b0)/b0
                    c0      = pi*fsum*t0
                    g0      = phi04*t0
                    cos10   = cos(-twopif0*dt0)
                    sinc20  = cos10*sin(c0)/c0
                    sum0    = g0*(sinc0+sinc20)
                    phif(i) = (sinc0+sum0)*cos(-twopif*dt0)
                 end do
                 if(tin<2) return
              end if
              m1 = m+1
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(8)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=nhtin,m,2
                    t0        = real(i,kind=dp)
                    dt0       = dt+t0
                    a0        = pi*f*t0
                    f0        = phi02*t0
                    sinc0     = f0*sin(a0)/a0
                    cos0      = cos(twopif0*dt0)
                    b0        = pi*fdif*t0
                    sinc10    = cos0*sin(b0)/b0
                    c0        = pi*fsum*t0
                    g0        = phi04*t0
                    cos10     = cos(-twopif0*dt0)
                    sinc20    = cos10*sin(c0)/c0
                    sum0      = g0*(sinc0+sinc20)
                    phif(i)   = (sinc0+sum0)*cos(-twopif*dt0)
                    t1        = real(i-1,kind=dp)
                    dt1       = dt+t1
                    a1        = pi*f*t1
                    f1        = phi02*t1
                    sinc1     = f1*sin(a1)/a1
                    cos1      = cos(twopif0*dt1)
                    b1        = pi*fdif*t1
                    sinc11    = cos1*sin(b1)/b1
                    c1        = pi*fsum*t1
                    g1        = phi04*t1
                    cos11     = cos(-twopif0*dt1)
                    sinc21    = cos11*sin(c1)/c1
                    sum1      = g1*(sinc1+sinc21)
                    phif(i-1) = (sinc1+sum1)*cos(-twopif*dt1)
                                     
              end do
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(8)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=m,htin,2
                    t0        = real(i,kind=dp)
                    dt0       = dt+t0
                    a0        = pi*f*t0
                    f0        = phi02*t0
                    sinc0     = f0*sin(a0)/a0
                    cos0      = cos(twopif0*dt0)
                    b0        = pi*fdif*t0
                    sinc10    = cos0*sin(b0)/b0
                    c0        = pi*fsum*t0
                    g0        = phi04*t0
                    cos10     = cos(-twopif0*dt0)
                    sinc20    = cos10*sin(c0)/c0
                    sum0      = g0*(sinc0+sinc20)
                    phif(i)   = (sinc0+sum0)*cos(-twopif*dt0)
                    t1        = real(i+1,kind=dp)
                    dt1       = dt+t1
                    a1        = pi*f*t1
                    f1        = phi02*t1
                    sinc1     = f1*sin(a1)/a1
                    cos1      = cos(twopif0*dt1)
                    b1        = pi*fdif*t1
                    sinc11    = cos1*sin(b1)/b1
                    c1        = pi*fsum*t1
                    g1        = phi04*t1
                    cos11     = cos(-twopif0*dt1)
                    sinc21    = cos11*sin(c1)/c1
                    sum1      = g1*(sinc1+sinc21)
                    phif(i+1) = (sinc1+sum1)*cos(-twopif*dt1)
                         
               end do
           end if
       end subroutine raster_mod_sinc_shifted_unroll_2x_r8 


       subroutine raster_mod_sinc_shifted_r4(phif,htin,f,phi0,fx,rho0,dt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_mod_sinc_shifted_r4
           !dir$ attributes forceinline ::   raster_mod_sinc_shifted_r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_mod_sinc_shifted_unroll_r4
           real(kind=sp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=sp),                         intent(in)  :: f
           real(kind=sp),                         intent(in)  :: phi0
           real(kind=sp),                         intent(in)  :: fx
           real(kind=sp),                         intent(in)  :: rho0
           real(kind=sp),                         intent(in)  :: dt
           real(kind=sp), parameter :: pi    = 3.14159265358979323846264338328_sp
           real(kind=sp), parameter :: twopi = 2.0_sp*pi
          
           real(kind=sp), automatic :: sinc0
           real(kind=sp), automatic :: sinc10
           real(kind=sp), automatic :: sinc20
           real(kind=sp), automatic :: cos0
           real(kind=sp), automatic :: cos10
           real(kind=sp), automatic :: a0
           real(kind=sp), automatic :: b0
           real(kind=sp), automatic :: t0
           real(kind=sp), automatic :: c0
           real(kind=sp), automatic :: sum0
           real(kind=sp), automatic :: f0
           real(kind=sp), automatic :: g0
           real(kind=sp), automatic :: dt0
           real(kind=sp), automatic :: fdif,fsum,phi02,phi04,twopif,twopif0
           integer(kind=i4) :: i,tin,nhtin
           tin    = htin*2
           nhtin  = -htin
           fdif   = f-fx
           fsum   = f+fx
           phi02  = 0.5_sp*phi0
           phi04  = 0.25_sp*phi0
           twopif = twopi*f
           twopif0=twopi*fx
           if(rho0/=0.5_sp) then
              call raster_flux_sinc_unroll_r4(phif,htin,f,phi0)
              return
           else
             
              
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(4)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=nhtin,0
                    t0        = real(i,kind=sp)
                    dt0       = dt+t0
                    a0        = pi*f*t0
                    f0        = phi02*t0
                    sinc0     = f0*sin(a0)/a0
                    cos0      = cos(twopif0*dt0)
                    b0        = pi*fdif*t0
                    sinc10    = cos0*sin(b0)/b0
                    c0        = pi*fsum*t0
                    g0        = phi04*t0
                    cos10     = cos(-twopif0*dt0)
                    sinc20    = cos10*sin(c0)/c0
                    sum0      = g0*(sinc0+sinc20)
                    phif(i)   = (sinc0+sum0)*cos(-twopif*dt0)
                                     
              end do
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(4)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=1,htin
                    t0        = real(i,kind=sp)
                    dt0       = dt+t0
                    a0        = pi*f*t0
                    f0        = phi02*t0
                    sinc0     = f0*sin(a0)/a0
                    cos0      = cos(twopif0*dt0)
                    b0        = pi*fdif*t0
                    sinc10    = cos0*sin(b0)/b0
                    c0        = pi*fsum*t0
                    g0        = phi04*t0
                    cos10     = cos(-twopif0*dt0)
                    sinc20    = cos10*sin(c0)/c0
                    sum0      = g0*(sinc0+sinc20)
                    phif(i)   = (sinc0+sum0)*cos(-twopif*dt0)
                                     
                   
               end do
           end if
       end subroutine raster_mod_sinc_shifted_r4 


       subroutine raster_mod_sinc_shifted_r8(phif,htin,f,phi0,fx,rho0,dt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  raster_mod_sinc_shifted_r8
           !dir$ attributes forceinline ::   raster_mod_sinc_shifted_r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  raster_mod_sinc_shifted_unroll_r8
           real(kind=dp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=dp),                         intent(in)  :: f
           real(kind=dp),                         intent(in)  :: phi0
           real(kind=dp),                         intent(in)  :: fx
           real(kind=dp),                         intent(in)  :: rho0
           real(kind=dp),                         intent(in)  :: dt
           real(kind=dp), parameter :: pi    = 3.14159265358979323846264338328_dp
           real(kind=dp), parameter :: twopi = 2.0_dp*pi
          
           real(kind=dp), automatic :: sinc0
           real(kind=dp), automatic :: sinc10
           real(kind=dp), automatic :: sinc20
           real(kind=dp), automatic :: cos0
           real(kind=dp), automatic :: cos10
           real(kind=dp), automatic :: a0
           real(kind=dp), automatic :: b0
           real(kind=dp), automatic :: t0
           real(kind=dp), automatic :: c0
           real(kind=dp), automatic :: sum0
           real(kind=dp), automatic :: f0
           real(kind=dp), automatic :: g0
           real(kind=dp), automatic :: dt0
           real(kind=dp), automatic :: fdif,fsum,phi02,phi04,twopif,twopif0
           integer(kind=i4) :: i,tin,nhtin
           tin    = htin*2
           nhtin  = -htin
           fdif   = f-fx
           fsum   = f+fx
           phi02  = 0.5_dp*phi0
           phi04  = 0.25_dp*phi0
           twopif = twopi*f
           twopif0=twopi*fx
           if(rho0/=0.5_dp) then
              call raster_flux_sinc_unroll_r8(phif,htin,f,phi0)
              return
           else
             
              
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(4)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=nhtin,0
                    t0        = real(i,kind=dp)
                    dt0       = dt+t0
                    a0        = pi*f*t0
                    f0        = phi02*t0
                    sinc0     = f0*sin(a0)/a0
                    cos0      = cos(twopif0*dt0)
                    b0        = pi*fdif*t0
                    sinc10    = cos0*sin(b0)/b0
                    c0        = pi*fsum*t0
                    g0        = phi04*t0
                    cos10     = cos(-twopif0*dt0)
                    sinc20    = cos10*sin(c0)/c0
                    sum0      = g0*(sinc0+sinc20)
                    phif(i)   = (sinc0+sum0)*cos(-twopif*dt0)
                                     
              end do
              !dir$ assume_aligned phif:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector vectorlength(8)
              !dir$ vector multiple_gather_scatter_by_shuffles 
              !dir$ vector always
              do i=1,htin
                    t0        = real(i,kind=dp)
                    dt0       = dt+t0
                    a0        = pi*f*t0
                    f0        = phi02*t0
                    sinc0     = f0*sin(a0)/a0
                    cos0      = cos(twopif0*dt0)
                    b0        = pi*fdif*t0
                    sinc10    = cos0*sin(b0)/b0
                    c0        = pi*fsum*t0
                    g0        = phi04*t0
                    cos10     = cos(-twopif0*dt0)
                    sinc20    = cos10*sin(c0)/c0
                    sum0      = g0*(sinc0+sinc20)
                    phif(i)   = (sinc0+sum0)*cos(-twopif*dt0)
                                     
                   
               end do
           end if
       end subroutine raster_mod_sinc_shifted_r8 

       ! The choose of specific unrolled version shall be based upon
       ! the prior performance measurement and assesment.
       subroutine raster_mod_sinc_shifted_exec_r4(phif,htin,f,phi0,fx,rho0,dt,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: raster_mod_sinc_shifted_exec_r4 
           real(kind=sp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=sp),                         intent(in)  :: f
           real(kind=sp),                         intent(in)  :: phi0
           real(kind=sp),                         intent(in)  :: fx
           real(kind=sp),                         intent(in)  :: rho0
           real(kind=sp),                         intent(in)  :: dt
           integer(kind=i4),                      intent(in)  :: unroll_cnt
          
           select case (unroll_cnt)
              case (16)
                 call raster_mod_sinc_shifted_unroll_16x_r4(phif,htin,f,phi0,fx,rho0,dt)
              case (8)
                 call raster_mod_sinc_shifted_unroll_8x_r4(phif,htin,f,phi0,fx,rho0,dt)
              case (4)
                 call raster_mod_sinc_shifted_unroll_4x_r4(phif,htin,f,phi0,fx,rho0,dt)
              case (2)
                 call raster_mod_sinc_shifted_unroll_2x_r4(phif,htin,f,phi0,fx,rho0,dt)
              case (0)
                 call raster_mod_sinc_shifted_r4(phif,htin,f,phi0,fx,rho0,dt)
              case default
                 return
            end select
      end subroutine raster_mod_sinc_shifted_exec_r4


      subroutine raster_mod_sinc_shifted_exec_r8(phif,htin,f,phi0,fx,rho0,dt,unroll_cnt)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: raster_mod_sinc_shifted_exec_r8 
           real(kind=dp), dimension(-htin:htin),  intent(out) :: phif
           integer(kind=i4),                      intent(in)  :: htin !shall be divisable by 2
           real(kind=dp),                         intent(in)  :: f
           real(kind=dp),                         intent(in)  :: phi0
           real(kind=dp),                         intent(in)  :: fx
           real(kind=dp),                         intent(in)  :: rho0
           real(kind=dp),                         intent(in)  :: dt
           integer(kind=i4),                      intent(in)  :: unroll_cnt
          
           select case (unroll_cnt)
              case (16)
                 call raster_mod_sinc_shifted_unroll_16x_r8(phif,htin,f,phi0,fx,rho0,dt)
              case (8)
                 call raster_mod_sinc_shifted_unroll_8x_r8(phif,htin,f,phi0,fx,rho0,dt)
              case (4)
                 call raster_mod_sinc_shifted_unroll_4x_r8(phif,htin,f,phi0,fx,rho0,dt)
              case (2)
                 call raster_mod_sinc_shifted_unroll_2x_r8(phif,htin,f,phi0,fx,rho0,dt)
              case (0)
                 call raster_mod_sinc_shifted_r8(phif,htin,f,phi0,fx,rho0,dt)
              case default
                 return
            end select
      end subroutine raster_mod_sinc_shifted_exec_r8





end module eos_sensor
