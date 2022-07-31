

module eos_noise_immune


!======================================================!
! Various characteristics of Electro-Optical Systems   !
! Based mainly on Yakushenko Y.G book (rus):           !
! "Methods of preserving a noise immunity in EOS"      !
!======================================================!
!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         eos_noise_immune
 !          
 !          Purpose:
 !                        Various characteristics of Electro-Optical Systems   
 !                        Based mainly on Yakushenko Y.G book (rus):           
 !                        "Methods of preserving a noise immunity in EOS" 
 !          History:
 !                        Date: 31-07-2022
 !                        Time: 15:04 GMT+2
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
 !                          Yakushenko Y.G:   "Methods of Preserving 
 !                                            The Noise Immunity in EOS"
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
   
   use mod_kinds,    only : i4,sp,dp
   use mod_vectypes, only : ZMM16r4_t, ZMM8r8_t
   public
   implicit none

   
     ! Major version
     integer(kind=i4),  parameter :: EOS_NOISE_IMMUNE_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: EOS_NOISE_IMMUNE_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: EOS_NOISE_IMMUNE_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: EOS_NOISE_IMMUNE_FULLVER =   &
            1000*EOS_NOISE_IMMUNE_MAJOR+100*EOS_NOISE_IMMUNE_MINOR+10*EOS_NOISE_IMMUNE_MICRO
     ! Module creation date
     character(*),        parameter :: EOS_NOISE_IMMUNE_CREATE_DATE = "31-07-2022 12:12 +00200 (SUN 31 JUL 2022 GMT+2)"
     ! Module build date
     character(*),        parameter :: EOS_NOISE_IMMUNE_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: EOS_NOISE_IMMUNE_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: EOS_NOISE_IMMUNE_SYNOPSIS    = "EOS noise immunity characteristics and models."

   ! Integral coefficients of soil cover radiance flux
   real(kind=dp), parameter, public :: soil_rad = 0.95_dp
   real(kind=dp), parameter, public :: grass_rad= 0.97_dp
   real(kind=dp), parameter, public :: water_rad= 0.96_dp
   real(kind=dp), parameter, public :: snow_rad = 0.92_dp
   real(kind=dp), parameter, public :: sand_rad = 0.89_dp
   real(kind=dp), parameter, public :: clay_rad = 0.85_dp
   
   ! Major planet radiation characteristics
   !                                                      delta lo,up: albedo: T(K)eff, T(K)max, T(K)avg
   real(kind=dp), dimension(6), parameter :: earth_rad   = [0.0_dp,0._dp,0.39_dp,255.0_dp,323.0_dp,270.0_dp]
   real(kind=dp), dimension(6), parameter :: mars_rad    = [0.017_dp,0.120_dp,1.148_dp,255.0_dp,307.0_dp,217.0_dp]
   real(kind=dp), dimension(6), parameter :: venus_rad   = [0.048_dp,0.314_dp,0.76_dp,329.0_dp,324.0_dp,229.0_dp]
   real(kind=dp), dimension(6), parameter :: mercury_rad = [0.023_dp,0.62_dp,0.058_dp,450.0_dp,625.0_dp,0.0_dp]
   real(kind=dp), dimension(6), parameter :: jupiter_rad = [0.150_dp,0.240_dp,0.51_dp,122.0_dp,145.0_dp,102.0_dp]
   real(kind=dp), dimension(6), parameter :: saturn_rad  = [0.072_dp,0.1_dp,0.5_dp,90.0_dp,107.0_dp,76.0_dp]

   contains

   
   ! Module of spatial-frequency spectrum of uniformly bright
   ! rectangle of sides a,b and brightness of L.
   pure elemental function rect_spectr_r4(L,a,b,omx,omy) result(S)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: rect_spectr_r4
        !dir$ forceinline :: rect_spectr_r4
        real(kind=sp),   intent(in) :: L
        real(kind=sp),   intent(in) :: a
        real(kind=sp),   intent(in) :: b
        real(kind=sp),   intent(in) :: omx
        real(kind=sp),   intent(in) :: omy
        real(kind=sp) :: S
        ! Locals
        real(kind=sp), automatic :: ha,hb,lt,rt,ft
        ha = a*0.5_sp
        hb = b*0.5_sp
        ft = L*a*b
        lt = sin(omx*ha)/omx*ha
        rt = sin(omy*hb)/omy*hb
        S  = ft*lt*rt
   end function rect_spectr_r4

   pure elemental function rect_spectr_r8(L,a,b,omx,omy) result(S)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: rect_spectr_r8
        !dir$ forceinline :: rect_spectr_r8
        real(kind=dp),   intent(in) :: L
        real(kind=dp),   intent(in) :: a
        real(kind=dp),   intent(in) :: b
        real(kind=dp),   intent(in) :: omx
        real(kind=dp),   intent(in) :: omy
        real(kind=dp) :: S
        ! Locals
        real(kind=dp), automatic :: ha,hb,lt,rt,ft
        ha = a*0.5_dp
        hb = b*0.5_dp
        ft = L*a*b
        lt = sin(omx*ha)/omx*ha
        rt = sin(omy*hb)/omy*hb
        S  = ft*lt*rt
   end function rect_spectr_r8


   ! Vectorized forms
   subroutine rect_spectr_zmm8r8(L,a,b,omx,omy,S)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: rect_spectr_zmm8r8
        !dir$ forceinline :: rect_spectr_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rect_spectr_zmm8r8
        type(ZMM8r8_t),  intent(in)  :: L
        type(ZMM8r8_t),  intent(in)  :: a
        type(ZMM8r8_t),  intent(in)  :: b
        type(ZMM8r8_t),  intent(in)  :: omx
        type(ZMM8r8_t),  intent(in)  :: omy
        type(ZMM8r8_t),  intent(out) :: S 
        ! Locals
        type(ZMM8r8_t), parameter :: half = ZMM8r8_t(0.5_dp)
        type(ZMM8r8_t), automatic :: ha,hb,ft,lt,rt,omxa,omya
        !dir$ attributes align : 64 :: ha,hb,ft,lt,rt
        ft.v = L.v*a.v*b.v
        ha.v = a.v*half.v
        omxa.v = omx.v*ha.v
        lt.v   = sin(omxa.v)/omxa.v
        hb.v = b.v*half.v
        omya.v = omy.v*hb.v
        rt.v   = sin(omya.v)/omya.v
        S.v    = ft.v*lt.v*rt.v
   end subroutine rect_spectr_zmm8r8


   subroutine rect_spectr_zmm16r4(L,a,b,omx,omy,S)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: rect_spectr_zmm16r4
        !dir$ forceinline :: rect_spectr_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rect_spectr_zmm16r4
        type(ZMM16r4_t),  intent(in)  :: L
        type(ZMM16r4_t),  intent(in)  :: a
        type(ZMM16r4_t),  intent(in)  :: b
        type(ZMM16r4_t),  intent(in)  :: omx
        type(ZMM16r4_t),  intent(in)  :: omy
        type(ZMM16r4_t),  intent(out) :: S 
        ! Locals
        type(ZMM16r4_t), parameter :: half = ZMM16r4_t(0.5_sp)
        type(ZMM16r4_t), automatic :: ha,hb,ft,lt,rt,omxa,omya
        !dir$ attributes align : 64 :: ha,hb,ft,lt,rt
        ft.v = L.v*a.v*b.v
        ha.v = a.v*half.v
        omxa.v = omx.v*ha.v
        lt.v   = sin(omxa.v)/omxa.v
        hb.v = b.v*half.v
        omya.v = omy.v*hb.v
        rt.v   = sin(omya.v)/omya.v
        S.v    = ft.v*lt.v*rt.v
   end subroutine rect_spectr_zmm16r4

   ! Module of spatial-frequency spectrum of uniformly bright
   ! disk of radius R and brightness of L.
   
   pure elemental function disc_spectr_r4(L,R,J1,omp) result(S)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: disc_spectr_r4
        !dir$ forceinline :: disc_spectr_r4
        real(kind=sp),   intent(in) :: L
        real(kind=sp),   intent(in) :: R
        real(kind=sp),   intent(in) :: J1
        real(kind=sp),   intent(in) :: omp
        real(kind=sp) :: S
        real(kind=sp), parameter :: pi = 3.14159265358979323846264338328_sp
        real(kind=sp), automatic :: ft,st
        ft = L*R*J1
        st = (pi*R*abs(omp))/omp
        S  = ft*st
   end function disc_spectr_r4


   pure elemental function disc_spectr_r8(L,R,J1,omp) result(S)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: disc_spectr_r8
        !dir$ forceinline :: disc_spectr_r8
        real(kind=dp),   intent(in) :: L
        real(kind=dp),   intent(in) :: R
        real(kind=dp),   intent(in) :: J1
        real(kind=dp),   intent(in) :: omp
        real(kind=dp) :: S
        real(kind=dp), parameter :: pi = 3.14159265358979323846264338328_dp
        real(kind=dp), automatic :: ft,st
        ft = L*R*J1
        st = (pi*R*abs(omp))/omp
        S  = ft*st
   end function disc_spectr_r8


   ! Vectorized forms
   subroutine disc_spectr_zmm8r8(L,R,J1,omp,S)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: disc_spectr_zmm8r8
        !dir$ forceinline :: disc_spectr_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: disc_spectr_zmm8r8  
       type(ZMM8r8_t),  intent(in)  :: L
       type(ZMM8r8_t),  intent(in)  :: R
       type(ZMM8r8_t),  intent(in)  :: J1
       type(ZMM8r8_t),  intent(in)  :: omp
       type(ZMM8r8_t),  intent(out) :: S
       type(ZMM8r8_t), parameter :: pi = ZMM8r8_t(3.14159265358979323846264338328_dp)
       type(ZMM8r8_t), automatic :: ft,st
       !dir$ attributes align : 64 :: ft,st
       ft.v = L.v*R.v*J1.v
       st.v = (pi.v*R.v*abs(omp.v))/(omp.v)
       S.v  = ft.v*st.v
   end subroutine disc_spectr_zmm8r8


   subroutine disc_spectr_zmm16r4(L,R,J1,omp,S)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: disc_spectr_zmm16r4
        !dir$ forceinline :: disc_spectr_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: disc_spectr_zmm16r4  
       type(ZMM16r4_t),  intent(in)  :: L
       type(ZMM16r4_t),  intent(in)  :: R
       type(ZMM16r4_t),  intent(in)  :: J1
       type(ZMM16r4_t),  intent(in)  :: omp
       type(ZMM16r4_t),  intent(out) :: S
       type(ZMM16r4_t), parameter :: pi = ZMM16r4_t(3.14159265358979323846264338328_sp)
       type(ZMM16r4_t), automatic :: ft,st
       !dir$ attributes align : 64 :: ft,st
       ft.v = L.v*R.v*J1.v
       st.v = (pi.v*R.v*abs(omp.v))/(omp.v)
       S.v  = ft.v*st.v
   end subroutine disc_spectr_zmm16r4


   ! Module of spatial-frequency spectrum of uniformly bright
   ! L 2D Gauss function
   pure elemental function spectr_gauss2d_r4(a,L,omx,omy) result(S)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: spectr_gauss2d_r4
        !dir$ forceinline :: spectr_gauss2d_r4
        real(kind=sp),   intent(in) :: a
        real(kind=sp),   intent(in) :: L
        real(kind=sp),   intent(in) :: omx
        real(kind=sp),   intent(in) :: omy
        real(kind=sp) :: S
        real(kind=sp), parameter :: npi = -3.14159265358979323846264338328_sp
        real(kind=sp), automatic :: lt,mt,rt
        lt = a*a*L
        mt = npi*a
        rt = omx*omx+omy*omy
        S  = lt*exp(mt*rt)
   end function spectr_gauss2d_r4


   pure elemental function spectr_gauss2d_r8(a,L,omx,omy) result(S)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: spectr_gauss2d_r8
        !dir$ forceinline :: spectr_gauss2d_r8
        real(kind=dp),   intent(in) :: a
        real(kind=dp),   intent(in) :: L
        real(kind=dp),   intent(in) :: omx
        real(kind=dp),   intent(in) :: omy
        real(kind=dp) :: S
        real(kind=dp), parameter :: npi = -3.14159265358979323846264338328_dp
        real(kind=dp), automatic :: lt,mt,rt
        lt = a*a*L
        mt = npi*a
        rt = omx*omx+omy*omy
        S  = lt*exp(mt*rt)
   end function spectr_gauss2d_r8


   ! Vectorized forms

   subroutine spectr_gauss2d_zmm8r8(a,L,omx,omy,S)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: spectr_gauss2d_zmm8r8
        !dir$ forceinline :: spectr_gauss2d_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: spectr_gauss2d_zmm8r8  
        type(ZMM8r8_t),   intent(in)  :: a
        type(ZMM8r8_t),   intent(in)  :: L
        type(ZMM8r8_t),   intent(in)  :: omx
        type(ZMM8r8_t),   intent(in)  :: omy
        type(ZMM8r8_t),   intent(out) :: S
        type(ZMM8r8_t), parameter :: npi = ZMM8r8_t(-3.14159265358979323846264338328_dp)
        type(ZMM8r8_t), automatic :: lt,mt,rt
        !dir$ attributes align : 64 :: lt,mt,rt
        lt.v = a.v*a.v*L.v
        mt.v = npi.v*a.v
        rt.v = omx.v*omx.v+omy.v*omy.v
        S.v  = lt.v*exp(mt.v*rt.v)
   end subroutine spectr_gauss2d_zmm8r8


   subroutine spectr_gauss2d_zmm16r4(a,L,omx,omy,S)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: spectr_gauss2d_zmm16r4
        !dir$ forceinline :: spectr_gauss2d_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: spectr_gauss2d_zmm16r4  
        type(ZMM16r4_t),   intent(in)  :: a
        type(ZMM16r4_t),   intent(in)  :: L
        type(ZMM16r4_t),   intent(in)  :: omx
        type(ZMM16r4_t),   intent(in)  :: omy
        type(ZMM16r4_t),   intent(out) :: S
        type(ZMM16r4_t), parameter :: npi = ZMM16r4_t(-3.14159265358979323846264338328_sp)
        type(ZMM16r4_t), automatic :: lt,mt,rt
        !dir$ attributes align : 64 :: lt,mt,rt
        lt.v = a.v*a.v*L.v
        mt.v = npi.v*a.v
        rt.v = omx.v*omx.v+omy.v*omy.v
        S.v  = lt.v*exp(mt.v*rt.v)
   end subroutine spectr_gauss2d_zmm16r4


   ! Probability distribution of background noise 
   ! two-dimensional impulses which amplitude are
   ! distributed normally
   
   pure elemental function source_amp_gaussd_r4(sigl,L,ml) result(pL)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: source_amp_gaussd_r4
        !dir$ forceinline :: source_amp_gaussd_r4
        real(kind=sp),  intent(in) :: sigl
        real(kind=sp),  intent(in) :: L
        real(kind=sp),  intent(in) :: ml
        real(kind=sp) :: pL
        real(kind=sp),  parameter :: s2pi = 2.506628274631000502415765284811_sp
        real(kind=sp), automatic :: lt,num,den,sigl2
        sigl2 = sigl*sigl
        lt    = 1.0_sp/(sigl*s2pi)
        num   = L-ml*L-ml
        den   = sigl2+sigl2
        pL    = lt*exp(-(num/den))  
   end function source_amp_gaussd_r4


   pure elemental function source_amp_gaussd_r8(sigl,L,ml) result(pL)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: source_amp_gaussd_r8
        !dir$ forceinline :: source_amp_gaussd_r8
        real(kind=dp),  intent(in) :: sigl
        real(kind=dp),  intent(in) :: L
        real(kind=dp),  intent(in) :: ml
        real(kind=dp) :: pL
        real(kind=dp),  parameter :: s2pi = 2.506628274631000502415765284811_dp
        real(kind=dp), automatic :: lt,num,den,sigl2
        sigl2 = sigl*sigl
        lt    = 1.0_dp/(sigl*s2pi)
        num   = L-ml*L-ml
        den   = sigl2+sigl2
        pL    = lt*exp(-(num/den))  
   end function source_amp_gaussd_r8


   subroutine source_amp_gaussd_zmm8r8(sigl,L,ml,pL)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: source_amp_gaussd_zmm8r8
        !dir$ forceinline :: source_amp_gaussd_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: source_amp_gaussd_zmm8r8  
        type(ZMM8r8_t),  intent(in)  :: sigl
        type(ZMM8r8_t),  intent(in)  :: L
        type(ZMM8r8_t),  intent(in)  :: ml
        type(ZMM8r8_t),  intent(out) :: pL
        type(ZMM8r8_t), parameter :: s2pi = ZMM8r8_t(2.506628274631000502415765284811_dp)
        type(ZMM8r8_t), parameter :: one  = ZMM8r8_t(1.0_dp)
        type(ZMM8r8_t), automatic :: lt,num,den,sigl2
        !dir$ attributes align : 64 :: lt,num,den,sigl2
        sigl2.v = sigl.v*sigl.v
        lt.v    = one.v/(sigl.v*s2pi.v)
        num.v   = L.v-ml.v*L.v-ml.v
        pL.v    = lt.v*exp(-(num.v/den.v))
   end subroutine source_amp_gauss2d_zmm8r8


   subroutine source_amp_gaussd_zmm16r4(sigl,L,ml,pL)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: source_amp_gaussd_zmm16r4
        !dir$ forceinline :: source_amp_gaussd_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: source_amp_gaussd_zmm16r4  
        type(ZMM16r4_t),  intent(in)  :: sigl
        type(ZMM16r4_t),  intent(in)  :: L
        type(ZMM16r4_t),  intent(in)  :: ml
        type(ZMM16r4_t),  intent(out) :: pL
        type(ZMM16r4_t), parameter :: s2pi = ZMM16r4_t(2.506628274631000502415765284811_sp)
        type(ZMM16r4_t), parameter :: one  = ZMM16r4_t(1.0_sp)
        type(ZMM16r4_t), automatic :: lt,num,den,sigl2
        !dir$ attributes align : 64 :: lt,num,den,sigl2
        sigl2.v = sigl.v*sigl.v
        lt.v    = one.v/(sigl.v*s2pi.v)
        num.v   = L.v-ml.v*L.v-ml.v
        pL.v    = lt.v*exp(-(num.v/den.v))
   end subroutine source_amp_gauss2d_zmm16r4



end module eos_noise_immune
