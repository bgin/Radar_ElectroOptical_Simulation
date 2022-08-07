

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

   ! Relevant derived types

   ! Blend filter which is of use to suppress the of-axial light sources
   ! to penetrate the 
   type, public :: blend_filter

         sequence
         real(kind=sp)    :: R_l ! radius of input blend window
         real(kind=sp)    :: R   ! radius of output blend window
         real(kind=sp)    :: L_b ! blend length
         real(kind=sp)    :: om  ! half of FOV
         real(kind=sp)    :: phi ! angle of blend direct irradiance
         real(kind=sp)    :: k   ! coefficient of diminishing irradiance power
         integer(kind=i4) :: btype !blend filter type i.e. [type 1 for k<=5*10^8,type 2 for k<=0.5*10^6,
                                   !                        type 3 for k<=10^5,  type 4 for k<=10^3]                        
   end type blend_filter

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


   ! Exponential Burger law for gamm wavelength radiation
   pure elemental function burger_law_r4(I0g,alphg,l) result(Ilg)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: burger_law_r4
        !dir$ forceinline :: burger_law_r4
        real(kind=sp),  intent(in) :: I0g     !Initial distance radiance power
        real(kind=sp),  intent(in) :: alphg   ! coefficient absorption 
        real(kind=sp),  intent(in) :: l       ! distance in km
        real(kind=sp) :: Ilg
        Ilg = I0g*exp(-alphg*l)
   end function burger_law_r4

 
   pure elemental function burger_law_r8(I0g,alphg,l) result(Ilg)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: burger_law_r8
        !dir$ forceinline :: burger_law_r8
        real(kind=dp),  intent(in) :: I0g     !Initial distance radiance power
        real(kind=dp),  intent(in) :: alphg   ! coefficient absorption 
        real(kind=dp),  intent(in) :: l       ! distance in km
        real(kind=dp) :: Ilg
        Ilg = I0g*exp(-alphg*l)
   end function burger_law_r8


   subroutine burger_law_zmm8r8(I0g,alphg,l,Ilg)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: burger_law_zmm8r8
        !dir$ forceinline :: burger_law_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: burger_law_zmm8r8
        type(ZMM8r8_t),  intent(in) :: I0g
        type(ZMM8r8_t),  intent(in) :: alphg
        type(ZMM8r8_t),  intent(in) :: l
        type(ZMM8r8_t),  intent(out) :: Ilg
               
        Ilg.v = I0g.v*exp(-alphg.v*l.v)
   end subroutine burger_law_zmm8r8


   subroutine burger_law_zmm16r4(I0g,alphg,l,Ilg)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: burger_law_zmm16r4
        !dir$ forceinline :: burger_law_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: burger_law_zmm16r4
        type(ZMM16r4_t),  intent(in) :: I0g
        type(ZMM16r4_t),  intent(in) :: alphg
        type(ZMM16r4_t),  intent(in) :: l
        type(ZMM16r4_t),  intent(out) :: Ilg
               
        Ilg.v = I0g.v*exp(-alphg.v*l.v)
   end subroutine burger_law_zmm16r4


   pure elemental function atmos_humid_r4(aref,T) result(aabs)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: atmos_humid_r4
        !dir$ forceinline :: atmos_humid_r4
        real(kind=sp), intent(in) :: aref !referential humidity (g*m^-3)
        real(kind=sp), intent(in) :: T ! Atmosphere temperature (K)
        real(kind=sp) :: T
        real(kind=sp), parameter :: c0 = 25.22_sp
        real(kind=sp), parameter :: c1 = 273.16_sp
        real(kind=sp), parameter :: c2 = 5.31_sp
        real(kind=sp), automatic :: ft,st,tt
        ft   = aref/T
        st   = c0*(T-c1)/T
        tt   = c2*log(T/c1)
        aabs = ft*exp(st-tt)
   end function atmos_humid_r4


   pure elemental function atmos_humid_r8(aref,T) result(aabs)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: atmos_humid_r8
        !dir$ forceinline :: atmos_humid_r8
        real(kind=dp), intent(in) :: aref !referential humidity (g*m^-3)
        real(kind=dp), intent(in) :: T ! Atmosphere temperature (K)
        real(kind=dp) :: T
        real(kind=dp), parameter :: c0 = 25.22_dp
        real(kind=dp), parameter :: c1 = 273.16_dp
        real(kind=dp), parameter :: c2 = 5.31_dp
        real(kind=dp), automatic :: ft,st,tt
        ft   = aref/T
        st   = c0*(T-c1)/T
        tt   = c2*log(T/c1)
        aabs = ft*exp(st-tt)
   end function atmos_humid_r8


   subroutine atmos_humid_zmm8r8(aref,T,aabs)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: atmos_humid_zmm8r8
        !dir$ forceinline :: atmos_humid_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: atmos_humid_zmm8r8
        type(ZMM8r8_t),  intent(in) :: aref
        type(ZMM8r8_t),  intent(in) :: T
        type(ZMM8r8_t),  intent(out) :: aabs
        type(ZMM8r8_t), parameter :: c0 = ZMM8r8_t(25.22_dp)
        type(ZMM8r8_t), parameter :: c1 = ZMM8r8_t(273.16_dp)
        type(ZMM8r8_t), parameter :: c2 = ZMM8r8_t(5.31_dp)
        type(ZMM8r8_t), automatic :: ft,st,tt
        !dir$ attributes align : 64 :: ft,st,tt
        ft.v = aref.v/T.v
        st.v = c0.v*(T.v-c1.v)/T.v
        tt.v = c2.v*log(T.v/c1.v)
        aabs.v = ft.v*exp(st.v-tt.v)
   end subroutine atmos_humid_zmm8r8


   subroutine atmos_humid_zmm16r4(aref,T,aabs)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: atmos_humid_zmm16r4
        !dir$ forceinline :: atmos_humid_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: atmos_humid_zmm16r4
        type(ZMM16r4_t),  intent(in) :: aref
        type(ZMM16r4_t),  intent(in) :: T
        type(ZMM16r4_t),  intent(out) :: aabs
        type(ZMM16r4_t), parameter :: c0 = ZMM16r4_t(25.22_sp)
        type(ZMM16r4_t), parameter :: c1 = ZMM16r4_t(273.16_sp)
        type(ZMM16r4_t), parameter :: c2 = ZMM16r4_t(5.31_sp)
        type(ZMM16r4_t), automatic :: ft,st,tt
        !dir$ attributes align : 64 :: ft,st,tt
        ft.v = aref.v/T.v
        st.v = c0.v*(T.v-c1.v)/T.v
        tt.v = c2.v*log(T.v/c1.v)
        aabs.v = ft.v*exp(st.v-tt.v)
   end subroutine atmos_humid_zmm16r4

  ! Blend filter related functions
  ! Compute radiance flux composed of uniform background
  ! and of-axial flux noise source.
  ! Yakushenkov book (rus) "ElectroOptical devices noise immunity"
  ! page 113 formula (6.1)
  pure elemental function background_flux(blend,A,O,theta,k1, &
                                      L,E,k2) result(Phi)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: background_flux
        !dir$ forceinline :: background_flux
        type(blend_filter),  intent(in) :: blend
        real(kind=sp),       intent(in) :: A     ! input lens area
        real(kind=sp),       intent(in) :: O     ! FoV
        real(kind=sp),       intent(in) :: theta ! coeff of optical 
        real(kind=sp),       intent(in) :: k1    ! system coefficient 1 
        real(kind=sp),       intent(in) :: L     ! brightness of uniform background
        real(kind=sp),       intent(in) :: E     ! irradiance by the of-axial point source
        real(kind=sp),       intent(in) :: k2    ! system coefficient 2
        real(kind=sp) :: Phi ! combined input flux
        real(kind=sp), automatic :: ft,st,tt
        real(kind=sp), automatic :: lk
        lk = blend.k
        ft = A*O*theta
        st = k1*L
        tt = E*(k2/lk)
        Phi= ft*(st+tt)
  end function background_flux


  ! Vectorized form of above function
  subroutine background_flux_zmm16r4(blend,A,O,theta,k1, &
                                 L,E,k2,Phi)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: background_flux_zmm16r4
        !dir$ forceinline :: background_flux_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: background_flux_zmm16r4
        type(blend_filter),  intent(in) :: blend
        type(ZMM16r4_t),     intent(in) :: A
        type(ZMM16r4_t),     intent(in) :: O
        type(ZMM16r4_t),     intent(in) :: theta
        real(kind=sp),       intent(in) :: k1
        type(ZMM16r4_t),     intent(in) :: L
        type(ZMM16r4_t),     intent(in) :: E
        real(kind=sp),       intent(in) :: k2
        type(ZMM16r4_t),     intent(out) :: Phi
        type(ZMM16r4_t), automatic :: ft,st,tt
        !dir$ attributes align : 64 :: ft,st,tt
        real(kind=sp), automatic :: lk
        lk = blend.k
        ft.v = A.v*O.v*theta.v
        st.v = k1*L.v
        tt.v = E.v*(k2/lk)
        Phi.v= ft.v*(st.v+tt.v)
  end subroutine background_flux_zmm16r4

  ! Compute schema K_1A formulae (6.5) 
  !$omp declare simd simdlen(4) 
  pure elemental function schema_K1A(r_ob,phi,omega,K,rho) result(K1A) 
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: schema_K1A
        !dir$ forceinline :: schema_K1A
        use omp_lib
        real(kind=sp),  intent(in) :: r_ob  ! coeff of total brightness of optical objective surfaces
        real(kind=sp),  intent(in) :: phi   ! angle of direct irradiance of blend surface
        real(kind=sp),  intent(in) :: omega ! half of FOV
        real(kind=sp),  intent(in) :: K     ! K=C/(Dtg(omega)) diaphragm objective number
        real(kind=sp),  intent(in) :: rho
        real(kind=sp) :: K1A
        real(kind=sp), automatic :: t0,t1,t2,t3,t4
        real(kind=sp), automatic :: KK,K2,tphi,tom,cphi,com
        tphi = tan(phi)
        KK   = K*K
        K2   = K+K
        tom  = tan(omega)
        t0   = (tphi+tom)*(tphi+tom)
        t1   = 0.25_sp*t0
        t2   = 1.0_sp+r_ob/(t1+1.0_sp)
        cphi = cos(phi)
        t3   = 2.0_sp+KK*rho
        com  = cos(omega)
        t4   = 1.0_sp+K2
        K1A  = t2+(cphi-com)*(t3/t4)
  end function schema_K1A


  ! Compute schema K_1B formulae (6.6)
  !$omp declare simd simdlen(4)
  pure elemental function schema_K1B(r_ob,phi,omega,K,r_k,rho) result(K1B) 
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: schema_K1B
        !dir$ forceinline :: schema_K1B
        use omp_lib
        real(kind=sp),  intent(in) :: r_ob  ! coeff of total brightness of optical objective surfaces
        real(kind=sp),  intent(in) :: phi   ! angle of direct irradiance of blend surface
        real(kind=sp),  intent(in) :: omega ! half of FOV
        real(kind=sp),  intent(in) :: K     ! K=C/(Dtg(omega)) diaphragm objective number
        real(kind=sp),  intent(in) :: rho
        real(kind=sp),  intent(in) :: r_k
        real(kind=sp) :: K1B
        real(kind=sp), automatic :: t0,t1,t2,t3,t4,KK4,t5
        real(kind=sp), automatic :: KK,K2,tphi,tom,cphi,com
        K2 = K*K
        tom  = tan(omega)
        t0   = (tphi+tom)*(tphi+tom)
        t1   = 0.25_sp*t0
        t2   = 1.0_sp+r_ob/(t1+1.0_sp)
        KK4  = 4.0_sp*K2
        KK4  = r_k/KK4
        cphi = cos(phi)
        t3   = K2*rho*rho
        t4   = 1.0_sp+K+K
        t5   = t4*t4
        com  = cos(omega)
        K1B  = t2+KK4+(com-cphi)*(t3/t4)
  end function schema_K1B


  subroutine schema_K1B_iterative(r_ob,phi,omega,K,r_k,rho,K1B,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: schema_K1B_iterative
        !dir$ forceinline :: schema_K1B_iterative
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: schema_K1B_iterative
        real(kind=sp),  intent(in) :: r_ob
        real(kind=sp), dimension(n), intent(in) :: phi
        real(kind=sp), dimension(n), intent(in) :: omega
        real(kind=sp), dimension(n), intent(in) :: K
        real(kind=sp),               intent(in) :: r_k
        real(kinf=sp),               intent(in) :: rho
        real(kind=sp), dimension(n), intent(out) :: K1B
        integer(kind=i4),            intent(in) :: n
        integer(kind=i4)
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned omega:64
        !dir$ assume_aligned K:64
        !dir$ assume_aligned K1B:64
        !dir$ vector aligned
        !$omp simd simdlen(4)
        do i=1, n
            K1B(i) = schema_K1B(r_ob,phi(i),omega(i),K(i),r_k,rho)
        end do
  end subroutine schema_K1B_iterative


  subroutine schema_K1A_iterative(r_ob,phi,omega,K,r_k,rho,K1B,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: schema_K1A_iterative
        !dir$ forceinline :: schema_K1A_iterative
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: schema_K1A_iterative
        real(kind=sp),  intent(in) :: r_ob
        real(kind=sp), dimension(n), intent(in) :: phi
        real(kind=sp), dimension(n), intent(in) :: omega
        real(kind=sp), dimension(n), intent(in) :: K
        real(kind=sp),               intent(in) :: rho
        real(kind=sp), dimension(n), intent(out) :: K1B
        integer(kind=i4),            intent(in) :: n
        integer(kind=i4)
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned omega:64
        !dir$ assume_aligned K:64
        !dir$ assume_aligned K1B:64
        !dir$ vector aligned
        !$omp simd simdlen(4)
        do i=1, n
            K1B(i) = schema_K1A(r_ob,phi(i),omega(i),K(i),rho)
        end do
  end subroutine schema_K1A_iterative


  !Coefficient of interference weakening (formula 6.12)
  pure elemental function k_weak_r4(q,k2,k1,E_in,L) result(k)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: k_weak_r4
        !dir$ forceinline :: k_weak_r4
        real(kind=sp),  intent(in) :: q
        real(kind=sp),  intent(in) :: k2
        real(kind=sp),  intent(in) :: k1
        real(kind=sp),  intent(in) :: E_in
        real(kind=sp),  intent(in) :: L
        real(kind=sp) :: k
        real(kind=sp), automatic :: t0,t1
        t0 = q*(k1/k2)
        t1 = E_in/L
        k = t0*t1
  end function k_weak_r4


  pure elemental function k_weak_r8(q,k2,k1,E_in,L) result(k)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: k_weak_r8
        !dir$ forceinline :: k_weak_r8
        real(kind=dp),  intent(in) :: q
        real(kind=dp),  intent(in) :: k2
        real(kind=dp),  intent(in) :: k1
        real(kind=dp),  intent(in) :: E_in
        real(kind=dp),  intent(in) :: L
        real(kind=dp) :: k
        real(kind=dp), automatic :: t0,t1
        t0 = q*(k1/k2)
        t1 = E_in/L
        k = t0*t1
  end function k_weak_r8


  ! Circular blend filters

  ! Formula 6.13
  pure elemental function dirrad_angle_r4(R,L,phi) result(da)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: dirrad_angle_r4
        !dir$ forceinline :: dirrad_angle_r4
        real(kind=sp), intent(in) :: R
        real(kind=sp), intent(in) :: L
        real(kind=sp), intent(in) :: phi
        real(kind=sp) :: da
        real(kind=sp), automatic :: t0,tom
        t0 = R+R/L
        tom = tan(om)
        da  = atan(t0+tom)
  end function dirrad_angle_r4

  pure elemental function dirrad_angle_r8(R,L,phi) result(da)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: dirrad_angle_r8
        !dir$ forceinline :: dirrad_angle_r8
        real(kind=dp), intent(in) :: R
        real(kind=dp), intent(in) :: L
        real(kind=dp), intent(in) :: phi
        real(kind=dp) :: da
        real(kind=dp), automatic :: t0,tom
        t0 = R+R/L
        tom = tan(om)
        da  = atan(t0+tom)
  end function dirrad_angle_r8


  ! Formula 6.14
  pure elemental function param_Lb_r4(R,phi,om) result(Lb)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: param_Lb_r4
        !dir$ forceinline :: param_Lb_r4
        real(kind=sp), intent(in) :: R
        real(kind=sp), intent(in) :: phi
        real(kind=sp), intent(in) :: om
        real(kind=sp) :: Lb
        real(kind=sp), automatic :: tphi,tom,R2
        R2 = R+R
        tphi = tan(phi)
        tom  = tan(om)
        Lb   = R2/(tphi-tom)
  end function param_Lb_r4

  
  pure elemental function param_Lb_r8(R,phi,om) result(Lb)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: param_Lb_r8
        !dir$ forceinline :: param_Lb_r8
        real(kind=dp), intent(in) :: R
        real(kind=dp), intent(in) :: phi
        real(kind=dp), intent(in) :: om
        real(kind=dp) :: Lb
        real(kind=dp), automatic :: tphi,tom,R2
        R2 = R+R
        tphi = tan(phi)
        tom  = tan(om)
        Lb   = R2/(tphi-tom)
  end function param_Lb_r8

  
  ! Formula 6.15
  pure elemental function param_Rl_r4(R,Lb,om) result(Rl)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: param_Rl_r4
        !dir$ forceinline :: param_Rl_r4
        real(kind=sp),  intent(in) :: R
        real(kind=sp),  intent(in) :: Lb
        real(kind=sp),  intent(in) :: om
        real(kind=sp) :: Rl
        real(kind=sp), automatic :: tom
        tom = tan(om)
        Rl  = R+Lb*tom
  end function param_Rl_r4


  pure elemental function param_Rl_r8(R,Lb,om) result(Rl)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: param_Rl_r8
        !dir$ forceinline :: param_Rl_r8
        real(kind=dp),  intent(in) :: R
        real(kind=dp),  intent(in) :: Lb
        real(kind=dp),  intent(in) :: om
        real(kind=dp) :: Rl
        real(kind=dp), automatic :: tom
        tom = tan(om)
        Rl  = R+Lb*tom
  end function param_Rl_r8


  ! Formula 6.16
  pure elemental function param_Db_r4(Rl,g) result(Db)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: param_Db_r4
        !dir$ forceinline :: param_Db_r4
        real(kind=sp),  intent(in) :: Rl
        real(kind=sp),  intent(in) :: g
        real(kind=sp) :: Db
        Db = 2.0_sp*(Rl+g)
  end function param_Db_r4


  pure elemental function param_Db_r8(Rl,g) result(Db)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: param_Db_r8
        !dir$ forceinline :: param_Db_r8
        real(kind=dp),  intent(in) :: Rl
        real(kind=dp),  intent(in) :: g
        real(kind=dp) :: Db
        Db = 2.0_dp*(Rl+g)
  end function param_Db_r8


  ! Formula 6.18
  subroutine E_out_r4(L_i,Om_i,Eout,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: E_out_r4
        !dir$ forceinline :: E_out_r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: E_out_r4
        use omp_lib
        real(kind=sp), dimension(n), intent(in)  :: L_i  ! brightness i-th part of blend filter
        real(kind=sp), dimension(n), intent(in)  :: Om_i ! angle subtended by irradiance if L_i
        real(kind=sp),               intent(out) :: Eout ! 
        integer(kind=i4),            intent(in)  :: n
        integer(kind=i4) :: i
        Eout = 0.0_sp
        !dir$ assume_aligned L_i:64,Om_i:64,Eout:64
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !$omp simd reduction(+:Eout)
        do i=1, n
           Eout = Eout+L_i(i)*Om_(i)
        end do
  end subroutine E_out_r4


  subroutine E_out_r8(L_i,Om_i,Eout,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: E_out_r8
        !dir$ forceinline :: E_out_r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: E_out_r8
        use omp_lib
        real(kind=dp), dimension(n), intent(in)  :: L_i  ! brightness i-th part of blend filter
        real(kind=dp), dimension(n), intent(in)  :: Om_i ! angle subtended by irradiance if L_i
        real(kind=dp),               intent(out) :: Eout ! 
        integer(kind=i4),            intent(in)  :: n
        integer(kind=i4) :: i
        Eout = 0.0_sp
        !dir$ assume_aligned L_i:64,Om_i:64,Eout:64
        !dir$ vector aligned
        !dir$ vector vectorlength(8)
        !$omp simd reduction(+:Eout)
        do i=1, n
           Eout = Eout+L_i(i)*Om_(i)
        end do
  end subroutine E_out_r8


  ! Formula 6.19
  pure elemental function param_Ekpi_r4(rho,Ecb,sig1,sig2,k) result(Ekpi)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: param_Ekpi_r4
        !dir$ forceinline :: param_Ekpi_r4
        real(kind=sp),  intent(in) :: rho
        real(kind=sp),  intent(in) :: Ecb
        real(kind=sp),  intent(in) :: sig1
        real(kind=sp),  intent(in) :: sig2
        real(kind=sp),  intent(in) :: k
        real(kind=sp) :: Ekpi
        real(kind=sp), automatic :: ssig1,ssig2,sin2s1,sin2s2,t0
        t0 = rho*Ecb
        ssig1  = sin(sig1)
        sin2s1 = ssig1*ssig1
        ssig2  = sin(sig2)
        sin2s2 = ssig2*ssig2
        Ekpi   = t0*(sin2s2-sin2s1)*k
  end function param_Ekpi_r4


  pure elemental function param_Ekpi_r8(rho,Ecb,sig1,sig2,k) result(Ekpi)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: param_Ekpi_r8
        !dir$ forceinline :: param_Ekpi_r8
        real(kind=dp),  intent(in) :: rho
        real(kind=dp),  intent(in) :: Ecb
        real(kind=dp),  intent(in) :: sig1
        real(kind=dp),  intent(in) :: sig2
        real(kind=dp),  intent(in) :: k
        real(kind=dp) :: Ekpi
        real(kind=dp), automatic :: ssig1,ssig2,sin2s1,sin2s2,t0
        t0 = rho*Ecb
        ssig1  = sin(sig1)
        sin2s1 = ssig1*ssig1
        ssig2  = sin(sig2)
        sin2s2 = ssig2*ssig2
        Ekpi   = t0*(sin2s2-sin2s1)*k
  end function param_Ekpi_r8


  subroutine param_Ekpi_zmm16r4(rho,Ecb,sig1,sig2,k,Ekpi)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: param_Ekpi_zmm16r4
        !dir$ forceinline :: param_Ekpi_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: param_Ekpi_zmm16r4
        type(ZMM16r4_t),  intent(in) :: rho
        type(ZMM16r4_t),  intent(in) :: Ecb
        type(ZMM16r4_t),  intent(in) :: sig1
        type(ZMM16r4_t),  intent(in) :: sig2
        type(ZMM16r4_t),  intent(in) :: k
        type(ZMM16r4_t),  intent(out) :: EKpi
        type(ZMM16r4_t), automatic :: ssig1,ssig2,sin2s1,sin2s2,t0
        !dir$ attributes align : 64 :: ssig1,ssig2,sin2s1,sin2s2,t0
        t0.v     = rho.v*Ecb.v
        ssig1.v  = sin(sig1.v)
        sin2s1.v = ssig1.v*ssig1.v
        ssig2.v  = sin(sig2.v)
        sin2s2.v = ssig2.v*ssig2.v
        Ekpi.v   = t0.v*(sin2s2.v-sin2s1.v)*k.v
  end subroutine param EKpi_zmm16r4


  subroutine param_Ekpi_zmm8r8(rho,Ecb,sig1,sig2,k,Ekpi)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: param_Ekpi_zmm8r8
        !dir$ forceinline :: param_Ekpi_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: param_Ekpi_zmm8r8
        type(ZMM8r8_t),  intent(in) :: rho
        type(ZMM8r8_t),  intent(in) :: Ecb
        type(ZMM8r8_t),  intent(in) :: sig1
        type(ZMM8r8_t),  intent(in) :: sig2
        type(ZMM8r8_t),  intent(in) :: k
        type(ZMM8r8_t),  intent(out) :: EKpi
        type(ZMM8r8_t), automatic :: ssig1,ssig2,sin2s1,sin2s2,t0
        !dir$ attributes align : 64 :: ssig1,ssig2,sin2s1,sin2s2,t0
        t0.v     = rho.v*Ecb.v
        ssig1.v  = sin(sig1.v)
        sin2s1.v = ssig1.v*ssig1.v
        ssig2.v  = sin(sig2.v)
        sin2s2.v = ssig2.v*ssig2.v
        Ekpi.v   = t0.v*(sin2s2.v-sin2s1.v)*k.v
  end subroutine param EKpi_zmm8r8

end module eos_noise_immune
