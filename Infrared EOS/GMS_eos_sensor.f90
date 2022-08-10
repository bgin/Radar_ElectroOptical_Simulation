

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
 !                         eos_noise_immune
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
   use mod_vectypes, only : ZMM16r4_t, ZMM8r8_t
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
     
     type, public :: scanning_mirror

           sequence
           real(kind=sp) :: gamma ! angle of mirror position relative to obiective optical axis (p. 53, 1.2)
           real(kind=sp) :: gamma0 ! angle of fixing
           real(kind=sp) :: phi   ! sensor fov
           real(kind=sp) :: F     ! Focal length
           real(kind=sp) :: H     ! distance from the focal length to target image
           real(kind=sp) :: Dmax  ! size of mirror vertical plane
           real(kind=sp) :: Dmin  ! size of mirror horizontal plane
           
     end type scanning_mirror


     contains

     
     subroutine param_gamma_r4(sm)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: param_gamma_r4
        !dir$ forceinline :: param_gamma_r4
        type(scanning_mirror), intent(inout) :: sm
        sm.gamma = 0.5_sp*sm.phi*0.5_sp
     end subroutine param_gamma_r4

     ! Formula 1, p.54
     !Тогда длина перпендикуляра SN, опущенного из 
     !светящейся точки на плоскость зеркала
     pure elemental function compute_SN_r4(R,phi,gamma) result(SN)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_r4
        !dir$ forceinline :: compute_SN_r4
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


     pure elemental function compute_SN_r8(R,phi,gamma) result(SN)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_r8
        !dir$ forceinline :: compute_SN_r8
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


     pure function compute_SN_zmm16r4(R,phi,gamma) result(SN)

        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_zmm16r4
        !dir$ forceinline :: compute_SN_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_zmm16r4
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  intent(in), optional :: phi
        type(ZMM16r4_t),  intent(in), optional :: gamma
        type(ZMM16r4_t) :: SN
        if(present(phi)) then
            SN.v = R.v*sin(phi.v)
        else if(present(gamma)) then
            SN.v = R.v*cos(gamma.v)
        end if
     end function compute_SN_zmm16r4

     
      pure function compute_SN_zmm8r8(R,phi,gamma) result(SN)

        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SN_zmm8r8
        !dir$ forceinline :: compute_SN_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_zmm8r8
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  intent(in), optional :: phi
        type(ZMM8r8_t),  intent(in), optional :: gamma
        type(ZMM8r8_t) :: SN
        if(present(phi)) then
            SN.v = R.v*sin(phi.v)
        else if(present(gamma)) then
            SN.v = R.v*cos(gamma.v)
        end if
     end function compute_SN_zmm8r8


     ! Formula 2, p. 54
     ! расстояние SM от светящейся точки до ее изображения
     pure elemental function compute_SM_r4(R,phi,gamma) result(SM)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_r4
        !dir$ forceinline :: compute_SM_r4
        real(kind=sp), intent(in) :: R
        real(kind=sp), intent(in) :: phi
        real(kind=sp), intent(in) :: gamma
        real(kind=sp) :: SM
        SM = 2.0_sp*compute_SN_r4(R,phi,gamma)
     end function compute_SM_r4
 

     pure elemental function compute_SM_r8(R,phi,gamma) result(SM)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_r8
        !dir$ forceinline :: compute_SM_r8
        real(kind=dp), intent(in) :: R
        real(kind=dp), intent(in) :: phi
        real(kind=dp), intent(in) :: gamma
        real(kind=dp) :: SM
        SM = 2.0_sp*compute_SN_r8(R,phi,gamma)
     end function compute_SM_r8

     
     pure function compute_SM_zmm16r4(R,phi,gamma) result(SM)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_zmm16r4
        !dir$ forceinline :: compute_SM_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_zmm16r4
        type(ZMM16r4_t),  intent(in) :: R
        type(ZMM16r4_t),  intent(in), optional :: phi
        type(ZMM16r4_t),  intent(in), optional :: gamma
        type(ZMM16r4_t) :: SM
        type(ZMM16r4_t), automatic :: SN
        !dir$ attributes align : 64 :: SN
        SN = compute_SN_zmm16r4(R,phi,gamma)
        SM = 2.0_sp*SN.v
     end function compute_SM_zmm16r4


     pure function compute_SM_zmm8r8(R,phi,gamma) result(SM)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_SM_zmm8r8
        !dir$ forceinline :: compute_SM_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_zmm8r8
        type(ZMM8r8_t),  intent(in) :: R
        type(ZMM8r8_t),  intent(in), optional :: phi
        type(ZMM8r8_t),  intent(in), optional :: gamma
        type(ZMM8r8_t) :: SM
        type(ZMM88r8_t), automatic :: SN
        !dir$ attributes align : 64 :: SN
        SN = compute_SN_zmm8r8(R,phi,gamma)
        SM = 2.0_sp*SN.v
     end function compute_SM_zmm8r8


     !Сканирующее зеркало для обеспечения осмотра всего поля
     !обзора ф необходимо повернуть на угол, обеспечивающий 
     !совмещение края изображения источника излучения с отверстием 
     !диафрагмы а, находящимся в центре поля. Для этого необходимо 
     !повернуть изображение светящейся точки S на угол ф/2
     ! Formula 1, p. 56
     pure elemental function ratio_FH_r4(psi,phi) result(FH)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_r4
        !dir$ forceinline :: ratio_FH_r4
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
        !dir$ forceinline :: ratio_FH_r8
        real(kind=dp),  intent(in) :: psi
        real(kind=dp),  intent(in) :: phi
        real(kind=dp) :: FH
        real(kind=dp), automatic :: hpsi,hphi
        hpsi = 0.5_dp*psi
        hphi = 0.5_dp*phi
        FH   = tan(hpsi)/tan(hphi)
     end function ratio_FH_r8


     pure function ratio_FH_zmm16r4(psi,phi) result(FH)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_zmm16r4
        !dir$ forceinline :: ratio_FH_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_zmm16r4
        type(ZMM16r4_t),  intent(in) :: psi
        type(ZMM16r4_t),  intent(in) :: phi
        type(ZMM16r4_t) :: FH
        type(ZMM16r4_t), parameter :: half = ZMM16r4_t(0.5_sp)
        type(ZMM16r4_t), automatic :: hpsi,hphi
        hpsi = half.v*psi.v
        hphi = half.v*phi.v
        FH   = tan(hpsi.v)/tan(hphi.v)
     end function ratio_FH_zmm16r4


     pure function ratio_FH_zmm8r8(psi,phi) result(FH)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: ratio_FH_zmm8r8
        !dir$ forceinline :: ratio_FH_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_zmm8r8
        type(ZMM8r8_t),  intent(in) :: psi
        type(ZMM8r8_t),  intent(in) :: phi
        type(ZMM8r8_t) :: FH
        type(ZMM8r8_t), parameter :: half = ZMM8r8_t(0.5_dp)
        type(ZMM8r8_t), automatic :: hpsi,hphi
        hpsi = half.v*psi.v
        hphi = half.v*phi.v
        FH   = tan(hpsi.v)/tan(hphi.v)
     end function ratio_FH_zmm8r8


     ! следовательно, угол установки сканирующего зеркала
     ! Formula 4, p. 56
     pure elemental function scan_mirror_ang_r4(gam0,psi,phi,dir) result(gamma)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_r4
        !dir$ forceinline :: scan_mirror_ang_r4
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
        !dir$ forceinline :: scan_mirror_ang_r8
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


     pure function scan_mirror_ang_zmm16r4(gam0,psi,phi,dir) result(gamma)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_zmm16r4
        !dir$ forceinline :: scan_mirror_ang_zmm16r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_zmm16r4
        type(ZMM16r4_t),  intent(in) :: gam0
        type(ZMM16r4_t),  intent(in) :: psi
        type(ZMM16r4_t),  intent(in) :: phi
        character(len=3), intent(in) :: dir
        type(ZMM16r4_t) :: gamma
        type(ZMM16r4_t), parameter :: half = ZMM16r4_t(0.5_sp)
        type(ZMM16r4_t), automatic :: t0,t1
        if(dir=="pos") then
            t0 = gam0.v+half.v*phi.v*half.v
            t1 = ratio_FH_zmm16r4(psi,phi)
            gamma = t0.v*t1.v
        else if(dir=="neg") then
            t0 = gam0.v-half.v*phi.v*half.v
            t1 = ratio_FH_zmm16r4(psi,phi)
            gamma = t0.v*t1.v
        end if
     end function scan_mirror_ang_zmm16r4


     pure function scan_mirror_ang_zmm8r8(gam0,psi,phi,dir) result(gamma)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_zmm8r8
        !dir$ forceinline :: scan_mirror_ang_zmm8r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_zmm8r8
        type(ZMM8r8_t),  intent(in) :: gam0
        type(ZMM8r8_t),  intent(in) :: psi
        type(ZMM8r8_t),  intent(in) :: phi
        character(len=3), intent(in) :: dir
        type(ZMM8r8_t) :: gamma
        type(ZMM8r8_t), parameter :: half = ZMM8r8_t(0.5_dp)
        type(ZMM8r8_t), automatic :: t0,t1
        if(dir=="pos") then
            t0 = gam0.v+half.v*phi.v*half.v
            t1 = ratio_FH_zmm8r8(psi,phi)
            gamma = t0.v*t1.v
        else if(dir=="neg") then
            t0 = gam0.v-half.v*phi.v*half.v
            t1 = ratio_FH_zmm8r8(psi,phi)
            gamma = t0.v*t1.v
        end if
     end function scan_mirror_ang_zmm8r8



     ! Maximum size of (verical) diameter of scanning mirror.
     ! Formula 2, page. 56, part: 1.3
     ! Anax = [h tg (6/2) + do6/2] [2 cos y' + sin yf {tg (у' + 6/2) +
     !+ tg(Y'-6/2)}].
     pure function compute_Dmax_r4(h,delta,d_ob,gamma) result(Dmax)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: compute_Dmax_r4
        !dir$ forceinline :: compute_Dmax_r4
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
        !dir$ forceinline :: compute_Dmax_r8
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








end module eos_sensor
